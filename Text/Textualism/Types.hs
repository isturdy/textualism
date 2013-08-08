{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Textualism.Types where

import           Control.Lens
import           Data.Map     (Map)
import           Data.Text    hiding (foldl')

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
              , rLabel    :: Maybe Text
              , rContentS :: [RSpan]
              }
            | RBLit {
                rLabel    :: Maybe Text
              , rClasses  :: [Text] -- May be empty
              , rContentT :: Text
              }
            | RBQuote {
                rLabel    :: Maybe Text
              , rCitation :: [RSpan]
              , rContentB :: [RBlock]
              }
            | RBPar {
                rLabel    :: Maybe Text
              , rContentS :: [RSpan]
              }
            | RBAligned {
                rAlignment :: Alignment
              , rLabel     :: Maybe Text
              , rLines     :: [[RSpan]] -- Outer level must never be empty
              }
            | RBMath {
                rLabel    :: Maybe Text
              , rContentT :: Text
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

data RSpan = RSQuote (Maybe Text) [RSpan]
           | RSLit [Text] Text
           | RSEm [RSpan]
           | RSText Text
           | RSMath MathType Text
           | RSRef LabelType Text
           | RIFn  [RSpan]
           | RILink LabelType Text
           deriving (Show)

data RDocument = RDocument Header [RBlock] Refs
               deriving (Show)

data Refs = Refs {
              _footnoteMap :: Map Text [RBlock]
            , _linkMap     :: Map Text Text
            } deriving (Show)
makeLenses ''Refs

-- The normalized document tree--substitution, numbering, etc.

data Block = BPar {
               label    :: Maybe Text
             , contentS :: [Span]
             }
           | BHeader {
               level    :: Int
             , label    :: Maybe Text
             , number   :: Int
             , contentS :: [Span]
             }
           | BQuote {
               label    :: Maybe Text
             , citation :: [Span]
             , contentB :: [Block]
             }
           | BLit {
               label    :: Maybe Text
             , classes  :: [Text] -- May be empty
             , contentT :: Text
             }
           | BMath {
               label    :: Maybe Text
             , contentT :: Text
             }
           | BAligned {
               alignment :: Alignment
             , label     :: Maybe Text
             , lines     :: [[Span]] -- Outer level must never be empty
             }
           | BHLine
           deriving (Show)

data Span = SText Text
          | SEm [Span]
          | SQuote (Maybe Text) [Span]
          | SLit [Text] Text
          | SFn Int
          | SMath MathType Text
          deriving (Show)

data Footnote = Footnote Int [Block]
              deriving (Show)

data Document = Document {
                  _header    :: Header
                , _footnotes :: [Footnote]
                , _blocks    :: [Block]
                } deriving (Show)
makeLenses ''Document
