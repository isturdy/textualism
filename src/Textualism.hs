module Main (main) where

import           Control.Applicative
import           Control.Lens                  hiding (argument)
import           Data.Default.Class
import           Data.Monoid
import           Data.Text.IO                  (hPutStrLn, readFile)
import           Data.Text.Lazy.IO             (writeFile)
import           Options.Applicative
import           Prelude                       hiding (readFile, writeFile)
import           System.FilePath.Lens
import           System.IO                     (stderr)
import           Text.Blaze.Html.Renderer.Text

import           Text.Textualism

data Opts = Opts {
              source :: String
            , target :: String
            }

opts :: Parser Opts
opts = Opts <$> argument str (metavar "SOURCEFILE")
            <*> strOption (
                   long "out"
                <> short 'o'
                <> value ""
                <> metavar "OUTFILE"
                <> help "Output file; defaults"
                )

parseOpts :: IO Opts
parseOpts = execParser $ info (helper <*> opts)
              (  fullDesc
              <> progDesc "convert SOURCEFILE"
              <> header "textualism - lightweight markup converter.")

main :: IO ()
main = do
  opt <- parseOpts
  let sourceFile = source opt
  let targetFile = case target opt of
        "" -> (extension .~ ".html" $ sourceFile)
        s  -> s
  doc <- readDocument <$> readFile sourceFile
  case doc of
    Left  err  -> hPutStrLn stderr err
    Right doc' -> writeFile targetFile . renderHtml
                  $ writeHtmlStandalone def doc'
