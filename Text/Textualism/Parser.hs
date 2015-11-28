{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Text.Textualism.Parser (
    parseDocument
  , parseSpans
  ) where

import           Control.Applicative
import           Control.Lens          hiding (noneOf)
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import qualified Data.List             as L
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text             hiding (count, filter, length, span)
import qualified Data.Text             as T
import           Prelude               hiding (span)
import           Text.Parsec           hiding (many, optional, parse, space,
                                        spaces, (<|>))
import           Text.Parsec.Text      ()

import           Text.Textualism.Types

type Parser = Parsec Text ParserState

data ParserState = ParserState {
    _indents :: [Int]
  , _refs    :: Refs
  , _fnNum   :: Int
  }
makeLenses ''ParserState

initialState :: ParserState
initialState = ParserState [0] (Refs mempty mempty) 0

getIndent :: Parser Int
getIndent = L.head . (^.indents) <$> getState

pushIndent :: Int -> Parser ()
pushIndent n = modifyState $ indents %~ (n:)

-- This is partial, but safe as now used (paired with pushIndent)
popIndent :: Parser ()
popIndent = modifyState $ indents %~ L.tail

addFn :: FNId -> [RBlock] -> Parser ()
addFn fnid fntxt = modifyState $ refs.footnoteMap %~ M.insert fnid fntxt

addLink :: Text -> Text -> Parser ()
addLink lid link = modifyState $ refs.linkMap %~ M.insert lid link

bumpFnNum :: Parser Int
bumpFnNum = modifyState (fnNum %~ succ) *> ((^.fnNum) <$> getState)

parse :: Parser a -> String -> Text -> Either ParseError a
parse p = runParser p initialState

parseDocument :: String -> Text -> Either Text RDocument
parseDocument source t = first (pack . show) (parse pDocument source t)

parseSpans :: Text -> Maybe [RSpan]
parseSpans = either (const Nothing) Just . parse pSpans ""

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

blankline :: Parser ()
blankline = try $ many space *> eol

--indent :: Parser Int
--indent = lookAhead (length <$> many litspace)

indentSame :: Parser ()
indentSame = try $ do
  i <- getIndent
  i' <- length <$> many litspace
  if i /= i' then fail $ "New indent (" <> show i <> "); expected " <> show i'
             else return ()

indentNew :: Parser ()
indentNew = void . try $ do
            i <- getIndent
            l <- length <$> many1 litspace
            if l > i then pushIndent l else fail "Indent Expected"

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

pDocument :: Parser RDocument
pDocument = RDocument <$> pMeta <*> pBlock <*> (_refs <$> getState)

pMeta :: Parser Meta
pMeta = option mempty $
        (try (string "~~~~~" *> eol)
          *> (M.fromList <$> many item))
         <*  string "~~~~~" <* eol <* blankline
  where item = (,) <$> (pack <$> many1 (noneOf "~:\r\n")
                   <*  char ':' <* many space)
                   <*> (pack <$> many1 (noneOf "\r\n"))
                   <*  eol

-- Block parsers

-- Parse a sequence of blocks at the present indentation
pBlock :: Parser [RBlock]
pBlock = filter notNil <$> sepBy (block <* many blankline) indentSame
  where block = pBlockRef <|> pBlockQuote <|> pBlockLit <|> pHeader
            <|> pAligned <|> pLine <|> pBlockMath <|> pBlockPar

-- Parse a sequence of blocks at a new (higher) indentation, and remove the
-- indent.
pNewBlock :: Parser [RBlock]
pNewBlock = indentNew *> pBlock <* popIndent

pNewLines :: Parser [[RSpan]]
pNewLines = (:) <$> (indentNew *> pSpans <* optional eol)
                <*> sepEndBy ((indentSame *> pSpans) <|> ([] <$ blankline)) eol
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
pBlockQuote = RBQuote <$> (pBlockSym ">" *> pBlockLabel)
                      <*> (pSpans <* eol)
                      <*> pNewBlock

pBlockPar :: Parser RBlock
pBlockPar = RBPar <$> pBlockLabel <*> pSpans1

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
                   <*> pBlockLabel <*> pSpans

pLine :: Parser RBlock
pLine = RBHLine <$ (try (string "-----") *> many (noneOf "\r\n") *> eol)

pBlockMath :: Parser RBlock
pBlockMath = RBMath <$> (pBlockSym "$$$" *> pBlockLabel) <*> pIndentedLines

pBlockRef :: Parser RBlock
pBlockRef = RBNil <$ (join . try $ char '[' *>
            (((addFn   <$ char '^') <*> (Right <$> pIdString <* string "]:\n")
                                    <*> pNewBlock)
         <|> ((addLink <$ char '@') <*> (pIdString <* string "]:" <* litspaces)
                                    <*> (pack <$> (many1 $ noneOf "\r\n"))
                                    <*  char '\n')))

-- Span parsers
pSpans :: Parser [RSpan]
pSpans = many pSpan

pSpansNoEm :: Parser [RSpan]
pSpansNoEm = many pSpanNoEm

pSpans1 :: Parser [RSpan]
pSpans1 = many1 pSpan

pSpan :: Parser RSpan
pSpan = pSpanNoEm <|> pEm

pSpanNoEm :: Parser RSpan
pSpanNoEm = pText <|> pQuote <|> pLit <|> pRef <|> pARef <|> pMath

pText :: Parser RSpan
pText = RSText . pack <$> many1 (escaped <|> notSpecial)
  where escaped = char '~' *> anyChar
        notSpecial = noneOf "`'<>\r\n[]*"

pQuote :: Parser RSpan
pQuote = RSQuote <$> (try (string "``")
                     *> optionMaybe (char '{' *> pIdString <* char '}' <* many space))
                <*> (pSpans <* string "''")

pLit :: Parser RSpan
pLit = RSLit <$> (char '`' *> option [] (char '{' *> pIdStrings <* char '}'))
            <*> (pLitText <* char '`')
  where pLitText = pack <$> many (try ('`' <$ string "``")
                                  <|> noneOf "`\r\n")

pMath :: Parser RSpan
pMath = do
  mtype <- char '$' *> option Inline (Display <$ char '$')
  RSMath mtype <$> (pack <$> many (noneOf "$\r\n"))
               <*  if mtype == Inline then char '$' else char '$' >> char '$'

pEm :: Parser RSpan
pEm = char '*' *> (RSEm <$> pSpansNoEm) <* char '*'

pDisplay :: Parser (Maybe [RSpan])
pDisplay = optionMaybe $ between (char '{') (char '}') pSpans

pRef :: Parser RSpan
pRef = char '[' *> (join . option ref $ (fn <$ char '^')
                    <|> (link <$ char '@'))
 where  rid = pIdString <* char ']'
        fn   = RSFn . Right <$> rid
        ref  = RSRef <$> rid <*> pDisplay
        link = RSLink <$> (Right <$> rid) <*> pDisplay

-- Anonymous references. A bit of code duplication; could this be partially
-- unified with above?
pARef :: Parser RSpan
pARef = char '<' *> (join $ (fn <$ char '^') <|> (link <$ char '@'))
  where fn = do fnid <- Left <$> bumpFnNum
                pure <$> RBPar Nothing <$> pSpans <*  char '>'
                  >>= addFn fnid
                return $ RSFn fnid
        link = RSLink <$> (Left . pack <$> many1 (noneOf "\r\n>") <* char '>')
                      <*> pDisplay
