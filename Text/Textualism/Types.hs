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

data Block = BHeader {
               level    :: Int
             , label    :: Maybe Label
             , contentS :: [Span]
             }
           | BLit {
               label    :: Maybe Label
             , classes  :: [Text] -- May be empty
             , contentT :: Text
             }
           | BQuote {
               label    :: Maybe Label
             , citation :: [Span]
             , contentB :: [Block]
             }
           | BPar {
               label    :: Maybe Label
             , contentS :: [Span]
             }
           | BAligned {
               alignment :: Alignment
             , label     :: Maybe Label
             , lines     :: [SLine] -- Must never be empty
             }
           | BMath {
               label    :: Maybe Label
             , contentT :: Text
             }
           | BMacro {
               bMacro     :: Text
             , bArguments :: [Text]
             , contentT   :: Text
             }
           | BHLine
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
               | CenteredLeft
               | AlignLeft
               | AlignRight
               deriving (Eq, Show)

data Span = SQuote Text [Span]
          | SLit [Text] Text
          | SEm
          | SMacro Text [Text]
          | SLabel LabelType Text
          | SText Text
          | SMath Text
          deriving (Show)

data SLine = Spans [Span]
           | NewLine
           deriving (Show)
