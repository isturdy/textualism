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
hdNum lvl = hNum <%= ((_last %~ succ) . take lvl . (<> repeat 0))

makeFn :: [RBlock] -> Norm Int
makeFn fn = do
  fn' <- traverse (normBlock False) fn
  n <- fnNum <<%= succ
  fns %= (Footnote n fn':)
  return n

left :: a -> StateT NormState (Either a) b
left = lift . Left

normalize :: RDocument -> Either Text Document
normalize (RDocument mt bs rfs) =
  flip evalStateT (NormState [] 1 rfs [] mempty) $
  Document mt
           (mt^.at "date" >>= parseTime defaultTimeLocale "%Y-%m-%d" . unpack)
       <$> traverse (traverse (normSpan True)) (mt^.at "title" >>= parseSpans)
       <*> traverse (normBlock True) bs
       <*> use fns

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
      lkp l i = use (l.at i)
                >>= maybe (left $ "Invalid reference: " <> i) pure
  in case s of
    RSQuote t ss  -> SQuote t <$> norm ss
    RSLit ts t    -> pure $ SLit ts t
    RSEm ss       -> SEm <$> norm ss
    RSText t      -> pure $ SText t
    RSMath tp t   -> pure $ SMath tp t
    RSRef t ss    -> SRef t <$> lkp hMap t <*> traverse norm ss
    RSLink lid ss -> SLink <$> either pure (lkp (refs.linkMap)) lid
                           <*> traverse norm ss
    RSFn fnid     ->
      if fnp
      then use (refs.footnoteMap.at fnid) >>= \x -> case x of
             Just fn -> SFn <$> makeFn fn
             Nothing -> left $ "Invalid footnote id: " <>
                                 (case fnid of
                                     Left  _ -> "bug in textualism"
                                     Right n -> pack $ show n)
      else left "Nested footnotes are strictly prohibited."
