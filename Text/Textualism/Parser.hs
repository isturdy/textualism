{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Text.Textualism.Parser where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import           Data.Text hiding (count, length)
import qualified Data.Text             as T
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Text ()
import Control.Lens

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

-- Random parsec utilities
litspace :: Parser ()
litspace = void $ char ' '

litspaces :: Parser ()
litspaces = void $ many1 (char ' ')

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' <* optional (char '\n'))

blankline :: Parser ()
blankline = many space *> eol

indent :: Parser Int
indent = lookAhead (length <$> many litspace)

indentSame :: Parser ()
indentSame = void $ getIndent >>= flip count litspace

indentNew :: Parser ()
indentNew = void $ do
            l <- liftM length (many litspace)
            i <- getIndent
            if l > i then pushIndent i else fail "Indent expected"

paragraphBreak :: Parser ()
paragraphBreak = eol *> blankline *> indentSame

notSpecial :: Parser Char
notSpecial = undefined

-- Block parsers
litBlock :: Parser Block
litBlock = BLit <$> header <*> (adjustIndents <$> indentedLines)
  where header = (try $ string "%%%") *> litspaces *> names
          where names = sepEndBy (pack <$> many1 (noneOf " \r\n")) litspaces
                        <* eol
        indentedLines = getIndent >>= \i ->
          sepEndBy (try (count i litspace) *> line) eol
          where line = pack <$> many (noneOf "\r\n")
        adjustIndents [] = mempty
        adjustIndents ls = T.unlines $ snd . T.splitAt minIndent <$> ls
          where minIndent = L.minimum $ T.length . fst . T.span (==' ') <$> ls

-- | Parse a line and push its indent onto the stack. Used for everything
-- except explicit blocks, which may have an indented first line.
newIndentLine :: Parser Block
newIndentLine = undefined
-- Pull the indents off a section of text.
--unindent :: Int -> Text -> Text
--unindent n = T.concat . fmap (T.take n) . T.lines
