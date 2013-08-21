module Text.Textualism (
    readDocument
  , writeHtml
  ) where

import           Control.Monad
import           Data.Text

import           Text.Textualism.Normalize
import           Text.Textualism.Parser
import           Text.Textualism.Types
import           Text.Textualism.Writers.Html

readDocument :: Text -> Either Text Document
readDocument = parseDocument "Source" >=> normalize
