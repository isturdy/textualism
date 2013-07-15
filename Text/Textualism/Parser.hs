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

notSpecial :: Parser Char
notSpecial = undefined

idString :: Parser Text
idString = pack <$> many1 (alphaNum <|> char '_' <|> char '\'')

pConstChar :: Char -> r -> Parser r
pConstChar c r = const r <$> char c

pBlockSym :: String -> Parser ()
pBlockSym t = void $ (try $ string t) *> many space

pLabel :: LabelType -> Parser (Maybe Label)
pLabel lt = optionMaybe . try $ Label <$>
        (char '[' *> typ) <*> (idString <* string "]:" <* spaces)
  where typ = option BlockLabel (pConstChar '^' FootnoteLabel)
        check l | l == lt = l
        check _ = error $ "Bad label; expecting " ++ (show lt)

pBlockLabel :: Parser (Maybe Label)
pBlockLabel = pLabel BlockLabel

-- Block parsers

-- Parse a sequence of blocks at the present indentation
pBlock :: Parser Block
pBlock = blocks <$> sepBy block indentSame
  where block = pBlockLit <|> pBlockQuote

-- Parse a sequence of blocks at a new (higher) indentation, and remove the
-- indent.
pNewBlock :: Parser Block
pNewBlock = indentNew *> pBlock <* popIndent

pNewLines :: Parser [Span]
pNewLines = (:) <$> (indentNew *> pSpan <* optional eol)
                <*> sepEndBy ((indentSame *> pSpan)
                              <|> (const NewLine <$> blankline))
                             eol
                <*  popIndent

-- Literal block. Note the non-standard indentation
-- (to allow for first-line indents)
pBlockLit :: Parser Block
pBlockLit = BLit <$> (pBlockSym "%%%" *> pBlockLabel)
                 <*> (sepEndBy idString litspaces <* eol)
                 <*> (adjustIndents <$> indentedLines)
  where indentedLines = (+1) <$> getIndent >>= \i ->
          sepEndBy (line i <|> const mempty <$> blankline) eol
          where line i = pack <$> ((try $ count i litspace)
                                   *> many (noneOf "\r\n"))
        adjustIndents [] = mempty
        adjustIndents ls = T.unlines $ T.drop minIndent <$> ls
          where minIndent = L.minimum $ fromMaybe maxBound
                                      . T.findIndex (/=' ') <$> ls

pBlockQuote :: Parser Block
pBlockQuote = BQuote <$> (pBlockSym ">" *> pBlockLabel) <*> pSpan <*> pBlock

pBlockPar :: Parser Block
pBlockPar = BPar <$> (many space *> pBlockLabel) <*> pSpan

-- Aligned block. Single newlines are significant.
pAligned :: Parser Block
pAligned = BAligned <$> (alignment <* spaces)
                    <*> (pBlockLabel <* eol)
                    <*> pNewLines
  where alignment = (try (string "|>") *> option AlignLeft
                     (const CenteredLeft <$> char '|'))
                <|> (try (string "<|") *> option AlignRight
                     (const Centered <$> char '>'))

pHeader :: Parser Block
pHeader = BHeader <$> (length <$> many1 (char '#') <* spaces)
                  <*> pBlockLabel <*> pSpan

pLine :: Parser Block
pLine = const BLine <$> (try (string "-----") *> many (noneOf "\r\n") *> eol)

-- Span parsers
pSpan :: Parser Span
pSpan = S <$> span
  where span = many (noneOf "\r\n")

-- Note lack of 'try'; absolutely no other parser may use '~'
pEscaped :: Parser Char
pEscaped = char '~' *> anyChar
