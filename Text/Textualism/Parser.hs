{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Text.Textualism.Parser where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.List as L
import           Data.Text hiding (count, length)
import qualified Data.Text             as T
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Text ()
import Control.Lens

import           Text.Textualism.Types

type Parser = Parsec Text ParserState

newtype Warning = Warning Text

data ParserState = ParserState {
    _indents  :: [Int]
  , _warnings :: [Warning] -- In reverse order for efficiency.
  }
makeLenses ''ParserState

initialState :: ParserState
initialState = ParserState [0] []

getIndent :: Parser Int
getIndent = liftM (L.head . (^.indents)) getState

pushIndent :: Int -> Parser ()
pushIndent n = modifyState (indents %~ (n:))

-- This is partial, but safe as now used (paired with pushIndent)
popIndent :: Parser ()
popIndent = modifyState (indents %~ L.tail)

warn :: Warning -> Parser ()
warn w = modifyState (warnings %~ (w:))

parse :: Parser a -> String -> Text -> Either ParseError a
parse p = runParser p initialState

-- Random parsec utilities
litspace :: Parser ()
litspace = void $ char ' '

litspaces :: Parser ()
litspaces = void $ many1 (char ' ')

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' <* optional (char '\n'))

blankline :: Parser ()
blankline = many space *> eol

indent :: Parser Int
indent = lookAhead (length <$> many litspace)

indentSame :: Parser ()
indentSame = void $ getIndent >>= flip count litspace

indentNew :: Parser ()
indentNew = void $ do
            l <- liftM length (many litspace)
            i <- getIndent
            if l > i then pushIndent i else fail "Indent expected"

paragraphBreak :: Parser ()
paragraphBreak = eol *> blankline *> indentSame

notSpecial :: Parser Char
notSpecial = undefined

idString :: Parser Text
idString = pack <$> many1 (alphaNum <|> char '_' <|> char '\'')

constChar :: Char -> a -> Parser a
constChar c r = const r <$> char c

ident :: Parser (Maybe Id)
ident = optionMaybe . try $
        char '[' *> typ <*> idString <* string "]:" <* litspaces
  where typ = option BlockId (constChar '^' FootnoteId)

blockId :: Parser (Maybe Id)
blockId = ident >>= test
  where test Nothing = return Nothing
        test (Just b@(BlockId _)) = return (Just b)
        test _ = fail "I expected a block id. What is this?"

-- Block parsers
litBlock :: Parser Block
litBlock = uncurry BLit <$> header <*> (adjustIndents <$> indentedLines)
  where header = (,) <$> ((try $ string "%%%") *> many litspace *> blockId)
                     <*> names
          where names = sepEndBy idString litspaces <* eol
        indentedLines = getIndent >>= \i ->
                          sepEndBy (try (count i litspace) *> line) eol
          where line = pack <$> many (noneOf "\r\n")
        adjustIndents [] = mempty
        adjustIndents ls = T.unlines $ snd . T.splitAt minIndent <$> ls
          where minIndent = L.minimum $ T.length . fst . T.span (==' ') <$> ls
