{-# LANGUAGE OverloadedStrings #-}

module Text.Textualism.Writers.Html (
    writeHtml
  , writeHtmlStandalone
  , WriterOptions(..)
  , writePre
  ) where

import           Control.Applicative
import           Control.Lens                hiding (pre)
import           Data.Char
import           Data.Default.Class
import           Data.Foldable
import qualified Data.List                   as L
import           Data.Monoid
import           Data.Text                   hiding (cons)
import           Data.Time.Format
import           Prelude                     hiding (div, mapM_, sequence_,
                                              unlines)
import           System.Locale
import           Text.Blaze.Html5            hiding (dt)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes hiding (id)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal         (Attributable, text, textValue)

import           Text.Textualism.Types

data WriterOptions = WriterOptions {
                       footnoteStyle :: FootnoteStyle
                     , htmlId        :: Maybe Text
                     , idPrefix      :: Text
                     , mathRenderer  :: MathRenderer
                     , titleLink     :: Maybe Text
                     , dateFormat    :: Maybe String
                     , timeLocale    :: TimeLocale
                     , highlighter   :: ([Text] -> Text -> Html)
                     }

instance Default WriterOptions where
  def = WriterOptions {
          footnoteStyle = Numbers
        , htmlId        = Nothing
        , idPrefix      = ""
        , mathRenderer  = MathJaxTex
        , titleLink     = Nothing
        , dateFormat    = Just "%e %b %y"
        , timeLocale    = defaultTimeLocale
        , highlighter   = writePre
        }

data FootnoteStyle = Numbers
                   | Letters
                   | Symbols
                   deriving (Show)

data MathRenderer = MathJaxTex
                  deriving (Show)

writePre :: [Text] -> Text -> Html
writePre cs t =
  pre !? (class_ . toValue . intercalate " " <$> nonEmpty cs) $ toHtml t
  where nonEmpty [] = Nothing
        nonEmpty l  = Just l

writeHtmlStandalone :: WriterOptions -> Document -> Html
writeHtmlStandalone cfg = docTypeHtml . snd . writeHtml cfg

writeHtml :: WriterOptions -> Document -> (Meta, Html)
writeHtml cfg (Document mt dt ttl bs fns) =
  let hd = case ttl of
        Nothing -> return ()
        Just t  -> let ttl' = mapM_ (writeSpan cfg) t
                   in H.header $ do
                     h1 $ maybe ttl'
                                (\lnk -> a ! href (toValue lnk) $ ttl')
                                (titleLink cfg)
                     case (dt, dateFormat cfg) of
                       (Just d, Just df) ->
                         let locale = timeLocale cfg
                         in  time ! datetime
                                      (toValue $ formatTime locale "%d %b %y" d)
                                  $ toHtml (formatTime locale df d)
                       _ -> return ()
  in (mt, article !? (A.id . toValue <$> htmlId cfg)
                  $ hd >> mapM_ (writeBlock cfg) bs >> writeFootnotes cfg fns)

writeBlock :: WriterOptions -> Block -> Html
writeBlock cfg bl =
  let writeB = mapM_ (writeBlock cfg)
      writeS = mapM_ (writeSpan cfg)
      mkId = A.id . toValue . mappend (idPrefix cfg)
  in case bl of
    BPar l ss          -> p !? (mkId <$> l) $ writeS ss
    BHeader lvl l _ ss -> hn lvl !? (mkId <$> l) $ writeS ss
    BQuote l cit cnt   -> div !? (mkId <$> l)
                              ! class_ (textValue "blockquote")
                              $ do
                                blockquote $ writeB cnt
                                p ! class_ (textValue "blockquote-citation")
                                  $ writeS cit
    BLit l cs t      -> ((highlighter cfg) cs t) !? (mkId <$> l)
    BMath l t        -> case mathRenderer cfg of
      MathJaxTex -> div !? (mkId <$> l)
                        !  class_ (textValue "math-display")
                        $  script ! type_ (textValue "math/tex")
                                  ! customAttribute "mode"
                                                    (textValue "display")
                                  $ toHtml t
    BAligned al l ls -> pre !? (mkId <$> l)
                            !  class_ (textValue . mappend "aligned "
                                       $ align al)
                            $ sequence_ . L.intersperse (toHtml '\n')
                              $ fmap writeS ls
    BHLine           -> hr

writeSpan :: WriterOptions -> Span -> Html
writeSpan cfg s =
  let write = mapM_ (writeSpan cfg)
      pref = mappend (idPrefix cfg)
      mkId = A.id . toValue . pref
  in case s of
    SText t     -> toMarkup t
    SEm ss      -> em $ write ss
    SQuote c ss -> q !? (class_ . toValue <$> c) $ write ss
    SLit cs t   -> case cs of
                     [] -> code $ toHtml t
                     _  -> code ! class_ (toValue $ intercalate " " cs)
                                $ toHtml t
    SFn n -> sup $ a ! (mkId . mappend "fnmark-" . pack $ show n)
                     ! class_ (textValue "fn-mark")
                     ! href (textValue . cons '#' . pref
                             . mappend "fn" . pack $ show n)
                     $ fnSymbol (footnoteStyle cfg) n
    SLink tgt ss -> a ! href (toValue tgt) $ maybe (toHtml tgt) write ss
    SRef rid num mss -> case mss of
      Just ss -> a ! href (toValue $ pref rid) $ write ss
      Nothing -> a ! href (toValue $ pref rid)
                   $ toHtml (L.concat . L.intersperse "." $ fmap show num)
    SMath tp t -> case mathRenderer cfg of
      MathJaxTex -> (if tp == Display
                     then (! customAttribute "mode" (textValue "display"))
                     else id)
                    $ script ! type_ (textValue "math/tex") $ toHtml t

writeFootnotes :: WriterOptions -> [Footnote] -> Html
writeFootnotes _cfg [] = return ()
writeFootnotes cfg fns = ol ! class_ (textValue "footnotes") $ mapM_ writeFn fns
  where
    pref = mappend (idPrefix cfg)
    mkId = A.id . toValue . pref
    bkref n = a ! class_ (textValue "fn-backref")
                ! href (textValue . mappend "#" . pref . mappend "fnmark-" . pack $ show n)
                $ text "^ "
    writeFn (Footnote n bs) = li ! (mkId . mappend "fn-" . pack $ show n)
                               $ bkref n >> mapM_ (writeBlock cfg) bs

align :: Alignment -> Text
align Centered     = "center"
align CenteredLeft = "centerleft"
align AlignLeft    = "left"
align AlignRight   = "right"

fnSymbol :: FootnoteStyle -> Int -> Html
fnSymbol Numbers = toHtml
fnSymbol Letters = toHtml . chr . (+96) . flip rem 26
fnSymbol Symbols = (fnSymbols !!) . flip rem 6 . (\x -> x - 1)

fnSymbols :: [Html]
fnSymbols = toHtml <$> ['*', '†', '‡', '§', '‖', '¶']

hn :: Int -> Html -> Html
hn 1 = h1
hn 2 = h2
hn 3 = h3
hn 4 = h4
hn 5 = h5
hn n | n > 5 = h6
hn _ = error "Text.Textualism.Writers.Html.hn: negative heading."

(!?) :: Attributable h => h -> Maybe Attribute -> h
(!?) h Nothing  = h
(!?) h (Just x) = h ! x
