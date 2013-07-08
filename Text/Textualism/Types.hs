{-# LANGUAGE OverloadedStrings #-}

module Text.Textualism.Types where

import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Text     hiding (foldl')

data Indent = Same
            | Dedent Int
            | Indent Int

data Document = Document Header [Block]
                deriving (Show)

-- | Lookup a value in a document's header.
lookupValue :: Text -> Document -> Maybe HValue
lookupValue k (Document h _) = M.lookup k h

type Header = Map Text HValue

data HValue = VList [Span]
            | VBlock Block
            | VExplicit Text
              deriving (Show)

data Block = Block
           | BSpan Span
           deriving (Show)

data Span = Span
          deriving (Show)
