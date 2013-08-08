{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Text.Textualism.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import qualified Data.List                 as L
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 hiding (count, filter, length, span)
import qualified Data.Text                 as T
import           Prelude                   hiding (span)
import           Text.Parsec               hiding (many, optional, parse, space,
                                            spaces, (<|>))
import           Text.Parsec.Text          ()

import           Text.Textualism.Normalize
import           Text.Textualism.Types
import           Text.Textualism.Util

type Parser = Parsec Text ParserState

newtype Warning = Warning Text

data ParserState = ParserState {
    _indents :: [Int]
  , _refs    :: Refs
  }
makeLenses ''ParserState

initialState :: ParserState
initialState = ParserState [] (Refs mempty mempty)

getIndent :: Parser Int
getIndent = liftM (L.head . (^.indents)) getState

pushIndent :: Int -> Parser ()
pushIndent n = modifyState (indents %~ (n:))

-- This is partial, but safe as now used (paired with pushIndent)
popIndent :: Parser ()
popIndent = modifyState (indents %~ L.tail)

addFn :: Text -> [RBlock] -> Parser ()
addFn fnid fntxt = modifyState (refs.footnoteMap %~ M.insert fnid fntxt)

addLink :: Text -> Text -> Parser ()
addLink lid link = modifyState (refs.linkMap %~ M.insert lid link)

parse :: Parser a -> String -> Text -> Either ParseError a
parse p = runParser p initialState

parseDocument :: String -> Text -> Either Text Document
parseDocument source t =
  mapLeft (pack . show) (parse pDocument source t) >>= normalize

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
blankline = try $ many space *> eol

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
pBlockSym t = void $ try (string t) *> many space

pLabel :: LabelType -> Parser (Maybe Text)
pLabel lt = (optionMaybe . try $
             (char '[' *> typ lt *> pIdString <* string "]:" <* spaces))
  where typ BlockLabel = return ()
        typ FootnoteLabel = void $ char '^'

pBlockLabel :: Parser (Maybe Text)
pBlockLabel = pLabel BlockLabel

-- Block parsers

pDocument :: Parser RDocument
pDocument = undefined

-- Parse a sequence of blocks at the present indentation
pBlock :: Parser [RBlock]
pBlock = filter notNil <$> sepBy block indentSame
  where block = pBlockPar <|> pBlockQuote <|> pBlockLit <|> pHeader <|> pAligned
            <|> pLine <|> pBlockMath <|> pBlockRef
-- Parse a sequence of blocks at a new (higher) indentation, and remove the
-- indent.
pNewBlock :: Parser [RBlock]
pNewBlock = indentNew *> pBlock <* popIndent

pNewLines :: Parser [[RSpan]]
pNewLines = (:) <$> (indentNew *> pSpan <* optional eol)
                <*> sepEndBy ((indentSame *> pSpan) <|> ([] <$ blankline)) eol
                <*  popIndent

pIndentedLines :: Parser Text
pIndentedLines = (+1) <$> getIndent >>= \i ->
  adjustIndents <$> sepEndBy (line i <|> (mempty <$ blankline)) eol
  where line i = pack <$> (try (count i litspace)
                           *> many (noneOf "\r\n"))
        adjustIndents [] = ""
        adjustIndents ls = T.unlines $ T.drop minIndent <$> ls
          where minIndent = L.minimum $ fromMaybe maxBound
                                      . T.findIndex (/=' ') <$> ls

-- Literal block. Note the non-standard indentation
-- (to allow for first-line indents)
pBlockLit :: Parser RBlock
pBlockLit = RBLit <$> (pBlockSym "```" *> pBlockLabel)
                  <*> (pIdStrings <* eol)
                  <*> pIndentedLines

pBlockQuote :: Parser RBlock
pBlockQuote = RBQuote <$> (pBlockSym ">" *> pBlockLabel) <*> pSpan <*> pBlock

pBlockPar :: Parser RBlock
pBlockPar = RBPar <$> (many space *> pBlockLabel) <*> pSpan

-- Aligned block. Single newlines are significant.
pAligned :: Parser RBlock
pAligned = RBAligned <$> (pAlignment <* spaces)
                     <*> (pBlockLabel <* eol)
                     <*> pNewLines
  where pAlignment = (try (string "|>") *> option AlignLeft
                     (CenteredLeft <$ char '|'))
                <|> (try (string "<|") *> option AlignRight
                     (Centered <$ char '>'))

pHeader :: Parser RBlock
pHeader = RBHeader <$> (length <$> many1 (char '#') <* spaces)
                   <*> pBlockLabel <*> pSpan

pLine :: Parser RBlock
pLine = RBHLine <$ (try (string "-----") *> many (noneOf "\r\n") *> eol)

pBlockMath :: Parser RBlock
pBlockMath = RBMath <$> (pBlockSym "$$$" *> pBlockLabel) <*> pIndentedLines

pBlockRef :: Parser RBlock
pBlockRef = RBNil <$ (join . try $ char '[' *>
            (((addFn   <$ char '^') <*> (pIdString <* string "]:\n")
                                    <*> pNewBlock)
         <|> ((addLink <$ char '@') <*> (pIdString <* string "]:" <* litspaces)
                                    <*> (pack <$> (many1 $ noneOf "\r\n"))
                                    <*  char '\n')))

-- Span parsers
pSpan :: Parser [RSpan]
pSpan = pSpanS ""

pSpanS :: String -> Parser [RSpan]
pSpanS s = many (pText s <|> pQuote <|> pEm <|> pLit <|> pMath)

pText :: String -> Parser RSpan
pText s =
  RSText . pack <$> many1 (escaped <|> notSpecial)
  where notSpecial = noneOf (s <> "`'<\r\n")
        escaped = char '~' *> anyChar

pQuote :: Parser RSpan
pQuote = RSQuote <$> (try (string "``")
                     *> optionMaybe (char '{' *> pIdString <* char '}'))
                <*> (pSpan <* string "''")

pLit :: Parser RSpan
pLit = RSLit <$> (char '`' *> option [] (char '{' *> pIdStrings <* char '}'))
            <*> (pLitText <* char '`')
  where pLitText = pack <$> many (try ('`' <$ string "``")
                              <|> noneOf "`\r\n")

pMath :: Parser RSpan
pMath = char '$' *> option Inline (Display <$ char '$') >>= \mtype ->
  RSMath mtype <$> (pack <$> many (noneOf "$\r\n"))
               <*  if mtype == Inline then char '$' else char '$' >> char '$'

pEm :: Parser RSpan
pEm = char '*' *> (RSEm <$> pSpanS "*") <* char '*'

pRef :: Parser RSpan
pRef = undefined
