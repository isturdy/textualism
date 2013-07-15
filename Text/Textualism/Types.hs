{-# LANGUAGE OverloadedStrings #-}

module Text.Textualism.Types where

import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Text hiding (foldl')

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

data Block = Blocks [Block]
           | BSpan Span
           | BHeader {
               level    :: Int
             , label    :: Maybe Label
             , contentS :: Span
             }
           | BLit {
               label    :: Maybe Label
             , classes  :: [Text] -- May be empty
             , contentT :: Text
             }
           | BQuote {
               label    :: Maybe Label
             , citation :: Span
             , contentB :: Block
             }
           | BPar {
               label    :: Maybe Label
             , contentS :: Span
             }
           | BAligned {
               alignment :: Alignment
             , label     :: Maybe Label
             , lines     :: [Span] -- Must never be empty
             }
           | BLine
           deriving (Show)

blocks :: [Block] -> Block
blocks [a] = a
blocks l   = Blocks l

data Label = Label {
               lType :: LabelType
             , lName :: Text
             }
           deriving (Eq, Show)

data LabelType = BlockLabel
               | FootnoteLabel
               deriving (Eq, Show)

data Alignment = Centered
               | CenteredLeft
               | AlignLeft
               | AlignRight
                 deriving (Eq, Show)

data Span = Spans [Span]
          | NewLine
          | S String
          deriving (Show)
