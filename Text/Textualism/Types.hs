{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Textualism.Types where

import           Control.Lens
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Text    hiding (foldl')

-- | Lookup a value in a document's header.
lookupValue :: Text -> RDocument -> Maybe RHValue
lookupValue k (RDocument h _ _) = M.lookup k h

-- Global types

data MathType = Display
              | Inline
              deriving (Eq, Show)

data LabelType = BlockLabel
               | FootnoteLabel
               deriving (Eq, Show)

data Alignment = Centered
               | CenteredLeft
               | AlignLeft
               | AlignRight
               deriving (Eq, Show)

-- The raw document tree--declaration order

type Header = Map Text RHValue

data RHValue = RVList [RSpan]
             | RVBlock RBlock
             | RVExplicit Text
             deriving (Show)

data RBlock = RBHeader {
                rLevel    :: Int
              , rLabel    :: Maybe Label
              , rContentS :: [RSpan]
              }
            | RBLit {
                rLabel    :: Maybe Label
              , rClasses  :: [Text] -- May be empty
              , rContentT :: Text
              }
            | RBQuote {
                rLabel    :: Maybe Label
              , rCitation :: [RSpan]
              , rContentB :: [RBlock]
              }
            | RBPar {
                rLabel    :: Maybe Label
              , rContentS :: [RSpan]
              }
            | RBAligned {
                rAlignment :: Alignment
              , rLabel     :: Maybe Label
              , rLines     :: [RSLine] -- Must never be empty
              }
            | RBMath {
                rLabel    :: Maybe Label
              , rContentT :: Text
              }
            | RBMacro {
                rBMacro     :: Text
              , rBArguments :: [Text]
              }
            | RBHLine
            | RBNil
            deriving (Show)

notNil :: RBlock -> Bool
notNil RBNil = False
notNil _     = True

data Label = Label {
               lType :: LabelType
             , lName :: Text
             } deriving (Eq, Show)

data RSpan = RSQuote Text [RSpan]
           | RSLit [Text] Text
           | RSEm [RSpan]
           | RSMacro Text [Text]
           | RSText Text
           | RSMath MathType Text
           | RSRef LabelType Text
           | RIFn  [RSpan]
           | RILink LabelType Text
           deriving (Show)

data RSLine = RSpans [RSpan]
            | RNewLine
            deriving (Show)

data RDocument = RDocument Header [RBlock] Refs
               deriving (Show)

data Refs = Refs {
              _footnoteMap :: Map Text [RBlock]
            , _linkMap     :: Map Text Text
            } deriving (Show)
makeLenses ''Refs

-- The normalized document tree--substitution, numbering, etc.

data Block = BHeader {
               level    :: Int
             , label    :: Maybe Label
             , number   :: Int
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
             }
           | BHLine
           deriving (Show)

data Span = SFn Int
          | SQuote Text [Span]
          | SLit [Text] Text
          | SEm [Span]
          | SText Text
          | SMath MathType Text
          | SMacro Text [Text]
          deriving (Show)

data Footnote = Footnote Int [Block]
              deriving (Show)

data SLine = Spans [Span]
           | NewLine
           deriving (Show)

data Document = Document {
                  _header    :: Header
                , _footnotes :: [Footnote]
                , _blocks    :: [Block]
                } deriving (Show)
makeLenses ''Document
