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
               label   :: Maybe Label
             , level   :: Int
             , content :: Span
             }
           | BLit {
               label   :: Maybe Label
             , classes :: [Text] -- May be empty
             , content :: Text
             }
           | BQuote {
               label    :: Maybe Label
             , content  :: Block
             , citation :: Span
             }
           | BPar {
               label   :: Maybe Label
             , content :: Span
             }
           | BAligned {
               alignment :: Alignment
             , label     :: Maybe Label
             , lines     :: [Span] -- Must never be empty
             }
           deriving (Show)

data Label = Label {
               lType :: LabelType
             , lName :: Text
             }
           deriving (Eq, Show)

data LabelType = BlockLabel
               | FootnoteLabel
               deriving (Eq, Show)

data Alignment = Centered
               | AlignLeft
               | AlignRight

-- These use strings because that is what even a text parser returns
toAlignment :: String -> Alignment
toAlignment "<|>" = Centered
toAlignment "|>"  = AlignLeft
toAlignment "<|"  = AlignRight
toAlignment _ = error "Text.Textualism.Types.toAlignment: Invalid alignment."

fromAlignment :: Alignment -> String
fromAlignment Centered   = "<|>"
fromAlignment AlignLeft  = "|>"
fromAlignment AlignRight = "<|"

data Citation = [Span]

data Span = Spans [Span]
          deriving (Show)
