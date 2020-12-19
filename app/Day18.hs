module Day18 where

import RIO hiding (try)
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO.List.Partial (head)

import Text.Megaparsec
    ( parse, errorBundlePretty, sepBy1, sepEndBy1, Parsec, ParseErrorBundle, try )
import Text.Megaparsec.Char ( char, hspace, eol )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

number :: Parser Int
number = lexeme L.decimal

mul :: Parser (Int -> Int -> Int)
mul = (*) <$ lexeme (char '*')

add :: Parser (Int -> Int -> Int)
add = (+) <$ lexeme (char '+')

parenExpr :: Parser Int -> Parser Int
parenExpr expr = do
    _ <- lexeme (char '(')
    x <- expr
    _ <- lexeme (char ')')
    return x

expr :: Parser Int -> Parser Int -> Parser (Int -> Int -> Int) -> Parser Int
expr lower total operator = do
    a <- try lower <|> parenExpr total
    cont a
    where cont a = do
            op <- optional operator
            case op of
                Nothing -> return a
                Just op' -> do b <- try lower <|> parenExpr total
                               cont (op' a b)

expr1 :: Parser Int
expr1 = expr number expr1 (add <|> mul)

addExpr :: Parser Int
addExpr = expr number mulExpr add

mulExpr :: Parser Int
mulExpr = expr addExpr mulExpr mul

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => Parser [Int] -> m [Int]
readInput p = do
    x <- parse p "data/day18.txt" <$> readFileUtf8 "data/day18.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

runA :: (HasLogFunc env) => RIO env ()
runA = do
    x <- readInput (expr1 `sepEndBy1` eol)
    logInfo $ display $ sum x

runB :: (HasLogFunc env) => RIO env ()
runB =  do
    x <- readInput (mulExpr `sepEndBy1` eol)
    logInfo $ display $ sum x
