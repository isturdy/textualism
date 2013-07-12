{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Text.Textualism.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import qualified Data.List             as L
import           Data.Monoid
import           Data.Text             hiding (count, length)
import qualified Data.Text             as T
import           Text.Parsec           hiding (many, optional, space, (<|>))
import           Text.Parsec.Text      ()

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

space :: Parser ()
space = satisfy test
  where test '\n' = False
        test '\r' = False
        test c    = isSpace c

spaces :: Parser ()
spaces = many space

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' <* optional (char '\n'))

blankline :: Parser ()
blankline = try $ many space *> eol

indent :: Parser Int
indent = lookAhead (length <$> many litspace)

indentSame :: Parser ()
indentSame = void . try $ getIndent >>= flip count litspace

indentNew :: Parser ()
indentNew = void $ do
            i <- getIndent
            l <- liftM length (try $ many litspace)
            if l > i then pushIndent i else fail "Indent expected"

paragraphBreak :: Parser ()
paragraphBreak = eol *> blankline *> indentSame

notSpecial :: Parser Char
notSpecial = undefined

idString :: Parser Text
idString = pack <$> many1 (alphaNum <|> char '_' <|> char '\'')

pConstChar :: Char -> r -> Parser r
pConstChar c r = const r <$> c

pBlockSym :: Text -> Parser ()
pBlockSym t = void $ (try $ string t) *> many space

pLabel :: Parser (Maybe Label)
pLabel = optionMaybe . try $
        char '[' *> typ <*> idString <* string "]:" <* litspaces
  where typ = option BlockId (pConst '^' FootnoteId)

blockId :: Parser (Maybe Id)
blockId = ident >>= test
  where test Nothing = return Nothing
        test (Just b@(BlockId _)) = return (Just b)
        test _ = fail "I expected a block id. What is this?"

-- Note lack of 'try'; absolutely no other parser may use '~'
pEscaped :: Parser Char
pEscaped = char '~' *> anyChar

-- Block parsers

-- Parse a sequence of blocks at the present indentation
pBlock :: Parser Block
pBlock = Blocks <$> sepBy block indentSame
  where block = pBlockLit <|> pBlockList

-- Parse a sequence of blocks at a new (higher) indentation, and remove the
-- indent.
pNewBlock :: Parser Block
pNewBlock = indentNew *> pBlock <* popIndent

-- Literal block. Note the non-standard indentation
-- (to allow for first-line indents)
pBlockLit :: Parser Block
pBlockLit = BLit <$> (pBlockSym "%%%" *> blockId)
                 <*> (sepEndBy idString litspaces <* eol)
                 <*> (adjustIndents <$> indentedLines)
  where indentedLines = getIndent >>= \i -> sepEndBy (line i <|> blankline) eol
          where line i = pack <$> try (count i litspace) *> many (noneOf "\r\n")
        adjustIndents [] = mempty
        adjustIndents ls = T.unlines $ T.drop minIndent <$> ls
          where minIndent = L.minimum $ T.findIndex (/=' ') <$> ls

pBlockQuote :: Parser Block
pBlockQuote = BQuote <$> (pBlockSym ">" *> blockId) <*> pSpan <*> pBlocks

pCitation :: Parser Citation
pCitation = Citation <$> pBlockSym "-" *> pSpan

pBlockPar :: Parser Block
pBlockPar = BPar <$> (many space *> pBlockLabel) <*> sepEndBy pSpan spaces

-- This is horrifically wrong, in ways that only a typechecker can fully
-- understand. (move constructor to first line, parse alignment correctly).
pAligned :: Parser Block
pAligned a = alignment >>= \a -> sepBy (aligned a) (eol *> indentSame)
  where aligned a = BAligned <$> a <*> pBlockLabel <*> pSpan <* eol
        alignment = lookAhead $ (const (pBlockSym "|>") <$> (string "|>"))
                    <|> (string "<|" *> option (pBlockSy "<|")
                         (const (pBlockSym "<|>") <$> char '>'))

pHeader :: Parser Block
pHeader = BHeader <$> (length <$> many1 (char '#') *> spaces)
                  <*> pBlockLabel <*> pSpan

-- Span parsers
pSpan :: Parser Span
pSpan = Spans <$> span
  where span = undefined
