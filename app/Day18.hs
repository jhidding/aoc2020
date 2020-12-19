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

lexeme = L.lexeme hspace
number = lexeme L.decimal

operator :: Parser (Int -> Int -> Int)
operator =  ((+) <$ lexeme (char '+'))
        <|> ((*) <$ lexeme (char '*'))

parenExpr :: Parser Int -> Parser Int
parenExpr expr = do
    _ <- lexeme (char '(')
    x <- expr
    _ <- lexeme (char ')')
    return x

expr :: Parser Int
expr = do
    a <- try number <|> parenExpr expr
    cont a
    where cont a = do
            op <- optional operator
            case op of
                Nothing -> return a
                Just op' -> do b <- try number <|> parenExpr expr
                               cont (op' a b)

sumExpr :: Parser Int
sumExpr = do
    a <- try number <|> parenExpr mulExpr
    cont a
    where cont a = do
            op <- optional ((+) <$ lexeme (char '+'))
            case op of
                Nothing -> return a
                Just op' -> do b <- try number <|> parenExpr mulExpr
                               cont (op' a b)

mulExpr :: Parser Int
mulExpr = do
    a <- try sumExpr <|> parenExpr mulExpr
    cont a
    where cont a = do
            op <- optional ((*) <$ lexeme (char '*'))
            case op of
                Nothing -> return a
                Just op' -> do b <- try sumExpr <|> parenExpr mulExpr
                               cont (op' a b)

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => Parser [Int] -> m [Int]
readInput p = do
    x <- parse p "data/day18.txt" <$> readFileUtf8 "data/day18.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

runA :: (HasLogFunc env) => RIO env ()
runA = do
    x <- readInput (expr `sepEndBy1` eol)
    logInfo $ display $ sum x

runB :: (HasLogFunc env) => RIO env ()
runB =  do
    x <- readInput (mulExpr `sepEndBy1` eol)
    logInfo $ display $ sum x
