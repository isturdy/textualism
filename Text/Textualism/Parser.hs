{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Text.Textualism.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import qualified Data.List             as L
import           Data.Maybe
import           Data.Monoid
import           Data.Text             hiding (count, length, span)
import qualified Data.Text             as T
import           Prelude               hiding (span)
import           Text.Parsec           hiding (many, optional, parse, space,
                                        spaces, (<|>))
import           Text.Parsec.Text      ()

import           Text.Textualism.Types

type Parser = Parsec Text ParserState

newtype Warning = Warning Text

data ParserState = ParserState {
    _indents  :: [Int]
  , _warnings :: [Warning] -- In reverse order for efficiency.
  }
makeLenses ''ParserState

initialState :: ParserState
initialState = ParserState [0] []

getIndent :: Parser Int
getIndent = liftM (L.head . (^.indents)) getState

pushIndent :: Int -> Parser ()
pushIndent n = modifyState (indents %~ (n:))

-- This is partial, but safe as now used (paired with pushIndent)
popIndent :: Parser ()
popIndent = modifyState (indents %~ L.tail)

warn :: Warning -> Parser ()
warn w = modifyState (warnings %~ (w:))

parse :: Parser a -> String -> Text -> Either ParseError a
parse p = runParser p initialState

-- Random parsec utilities
litspace :: Parser ()
litspace = void $ char ' '

litspaces :: Parser ()
litspaces = void $ many1 (char ' ')

space :: Parser ()
space = void $ satisfy test
  where test '\n' = False
        test '\r' = False
        test c    = isSpace c

spaces :: Parser ()
spaces = void $ many space

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' <* optional (char '\n'))

optionEol :: Parser ()
optionEol = option () eol

blankline :: Parser ()
blankline = try $ many space *> lookAhead eol

indent :: Parser Int
indent = lookAhead (length <$> many litspace)

indentSame :: Parser ()
indentSame = void . try $ getIndent >>= flip count litspace

indentNew :: Parser ()
indentNew = void . try $ do
            i <- getIndent
            l <- length <$> many1 litspace
            if l > i then pushIndent l else fail "Indent Expected"

paragraphBreak :: Parser ()
paragraphBreak = eol *> blankline *> indentSame

pIdString :: Parser Text
pIdString = pack <$> many1 (alphaNum <|> char '_' <|> char '\'')

pIdStrings :: Parser [Text]
pIdStrings = sepEndBy pIdString litspaces

pBlockSym :: String -> Parser ()
pBlockSym t = void $ (try $ string t) *> many space

pLabel :: LabelType -> Parser (Maybe Label)
pLabel lt = (optionMaybe . try $ Label <$>
             (char '[' *> typ) <*> (pIdString <* string "]:" <* spaces))
            >>= check
  where typ = option BlockLabel (FootnoteLabel <$ char '^')
        check Nothing = return Nothing
        check (Just l) = if lType l == lt then return (Just l)
                         else fail $ "Bad label; expecting " ++ (show lt)

pBlockLabel :: Parser (Maybe Label)
pBlockLabel = pLabel BlockLabel

-- Block parsers

-- Parse a sequence of blocks at the present indentation
pBlock :: Parser [Block]
pBlock = sepBy block indentSame
  where block = pBlockLit <|> pBlockQuote

-- Parse a sequence of blocks at a new (higher) indentation, and remove the
-- indent.
pNewBlock :: Parser [Block]
pNewBlock = indentNew *> pBlock <* popIndent

pNewLines :: Parser [SLine]
pNewLines = (:) <$> (indentNew *> (Spans <$> pSpan) <* optional eol)
                <*> sepEndBy (Spans <$> (indentSame *> pSpan)
                              <|> (NewLine <$ blankline))
                             eol
                <*  popIndent

-- Literal block. Note the non-standard indentation
-- (to allow for first-line indents)
pBlockLit :: Parser Block
pBlockLit = BLit <$> (pBlockSym "%%%" *> pBlockLabel)
                 <*> (pIdStrings <* eol)
                 <*> (pIndentedLines)

pIndentedLines :: Parser Text
pIndentedLines = (+1) <$> getIndent >>= \i ->
  adjustIndents <$> sepEndBy (line i <|> (mempty <$ blankline)) eol
  where line i = pack <$> ((try $ count i litspace)
                           *> many (noneOf "\r\n"))
        adjustIndents [] = ""
        adjustIndents ls = T.unlines $ T.drop minIndent <$> ls
          where minIndent = L.minimum $ fromMaybe maxBound
                                      . T.findIndex (/=' ') <$> ls

pBlockQuote :: Parser Block
pBlockQuote = BQuote <$> (pBlockSym ">" *> pBlockLabel) <*> pSpan <*> pBlock

pBlockPar :: Parser Block
pBlockPar = BPar <$> (many space *> pBlockLabel) <*> pSpan

-- Aligned block. Single newlines are significant.
pAligned :: Parser Block
pAligned = BAligned <$> (pAlignment <* spaces)
                    <*> (pBlockLabel <* eol)
                    <*> pNewLines
  where pAlignment = (try (string "|>") *> option AlignLeft
                     (CenteredLeft <$ char '|'))
                <|> (try (string "<|") *> option AlignRight
                     (Centered <$ char '>'))

pHeader :: Parser Block
pHeader = BHeader <$> (length <$> many1 (char '#') <* spaces)
                  <*> pBlockLabel <*> pSpan

pLine :: Parser Block
pLine = BHLine <$ (try (string "-----") *> many (noneOf "\r\n") *> eol)

pBlockMath :: Parser Block
pBlockMath = BMath <$> (pBlockSym "$$$" *> pBlockLabel) <*> pIndentedLines

-- Span parsers
pSpan :: Parser [Span]
pSpan = many span
  where span = pText <|> pQuote <|> pLit <|> pMath

pText :: Parser Span
pText = SText . pack <$> many1 (escaped <|> notSpecial
                            <|> (try (char '\'' <* notFollowedBy (char '\''))))
  where notSpecial = noneOf "`'%\r\n"
        escaped = (char '~' *> anyChar)

pQuote :: Parser Span
pQuote = SQuote <$> (try (string "``")
                     *> option mempty (char '{' *> pIdString <* char '}'))
                <*> (pSpan <* string "''")

pLit :: Parser Span
pLit = SLit <$> (char '`' *> option [] (char '{' *> pIdStrings <* char '}'))
            <*> (pLitText <* char '`')
  where pLitText = pack <$> many (try ('`' <$ string "``")
                              <|> noneOf "`\r\n")

pMath :: Parser Span
pMath = SMath . pack <$> (char '$' *> many (noneOf "$\r\n") <* char '$')
