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
eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' <* optional (char '\n'))

indent :: Parser Int
indent = lookAhead (length <$> many (char ' '))

notSpecial :: Parser Char
notSpecial = undefined

-- | Parse a line and push its indent onto the stack. Used for everything
-- except explicit blocks, which may have an indented first line.
newIndentLine :: Parser Block
newIndentLine =
-- Pull the indents off a section of text.
--unindent :: Int -> Text -> Text
--unindent n = T.concat . fmap (T.take n) . T.lines
