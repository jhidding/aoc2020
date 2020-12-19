module Day19 where

import RIO hiding (try)
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map.Partial
import RIO.List.Partial (head)

import Text.Megaparsec
    ( parse, errorBundlePretty, sepBy1, sepEndBy1, Parsec, ParseErrorBundle, try, anySingle, takeWhile1P )
import Text.Megaparsec.Char ( char, hspace, eol, string )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

data Rule
    = CharRule Char
    | SubRule [[Int]]
    deriving (Show)

type Rules = Map Int Rule

lexeme = L.lexeme hspace
integer = lexeme L.decimal

rule :: Parser (Int, Rule)
rule = do
    i <- integer
    string ": "
    r <- try     (CharRule <$> do { char '"'; x <- anySingle; char '"'; return x})
             <|> (SubRule <$> some integer `sepBy1` lexeme (char '|'))
    return (i, r)

rules :: Parser Rules
rules = Map.fromList <$> rule `sepEndBy1` eol

messages :: Parser [Text]
messages = takeWhile1P Nothing (`elem` ['a', 'b']) `sepEndBy1` eol

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => Parser a -> m a
readInput p = do
    x <- parse p "data/day19.txt" <$> readFileUtf8 "data/day19.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

runA :: (HasLogFunc env) => RIO env ()
runA = do
    (r, m) <- readInput (do { x <- rules; eol; y <- messages; return (x, y) })
    logInfo $ display $ tshow (r, m)

runB :: (HasLogFunc env) => RIO env ()
runB = logInfo "NYI"
