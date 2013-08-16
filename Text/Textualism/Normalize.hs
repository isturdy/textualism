{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.Textualism.Normalize (
    normalize
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import           Data.Monoid
import           Data.Text                  hiding (reverse, take)
import           Data.Time.Format
import           System.Locale

import           Text.Textualism.Parser
import           Text.Textualism.Types

data NormState = NormState {
                   _fns   :: [Footnote]
                 , _fnNum :: Int
                 , _refs  :: Refs
                 , _hNum  :: [Int] -- Present hierarchical section numbers
                 , _hMap  :: Map Text [Int] -- Labelled sections
                 }
makeLenses ''NormState

type Norm = StateT NormState (Either Text)

hdNum :: Int -> Norm [Int]
hdNum lvl = hNum <%= ((_last %~ succ) . take lvl)

makeFn :: [RBlock] -> Norm Int
makeFn fn = do
  fn' <- traverse (normBlock False) fn
  n <- fnNum <<%= succ
  fns %= (Footnote n fn':)
  return n

normalize :: RDocument -> Either Text Document
normalize (RDocument mt bs rfs) =
  flip evalStateT (NormState [] 1 rfs [] mempty) $
  Document mt
           (mt^.at "date" >>= parseTime defaultTimeLocale "%Y-%m-%d" . unpack)
       <$> traverse (traverse (normSpan True)) (mt^.at "title" >>= parseSpans)
       <*> traverse (normBlock True) bs
       <*> use fns
       <*> use hMap

normBlock :: Bool -> RBlock -> Norm Block
normBlock fnp b =
  let normS = traverse (normSpan fnp)
      {-# INLINE normS #-}
      normB = traverse (normBlock fnp)
      {-# INLINE normB #-}
  in case b of
    RBHeader lvl l ss -> BHeader lvl l <$> hdNum lvl <*> normS ss
    RBLit l cs c      -> pure $ BLit l cs c
    RBQuote l c bs    -> BQuote l <$> normS c <*> normB bs
    RBPar l ss        -> BPar l <$> normS ss
    RBAligned a l ls  -> BAligned a l <$> traverse normS ls
    RBMath l ct       -> pure $ BMath l ct
    RBHLine           -> pure BHLine
    RBNil             -> error "Bug at Text.Textualism.Normalize.normBlock: \
                               \RBNil escaped parser."

normSpan :: Bool -> RSpan -> Norm Span
normSpan fnp s =
  let norm = traverse (normSpan fnp)
      {-# INLINE norm #-}
  in case s of
    RSQuote t ss -> SQuote t <$> norm ss
    RSLit ts t   -> pure $ SLit ts t
    RSEm ss      -> SEm <$> norm ss
    RSText t     -> pure $ SText t
    RSMath tp t  -> pure $ SMath tp t
    RSFn fnid    ->
      if fnp
      then use (refs.footnoteMap.at fnid) >>= \x -> case x of
             Just fn -> SFn <$> makeFn fn
             Nothing -> lift . Left $ "Invalid footnote id: "
                                      <> pack (show fnid)
      else lift $ Left "Nested footnotes are strictly prohibited."
