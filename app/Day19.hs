{-# LANGUAGE TupleSections #-}
module Day19 where

import RIO hiding (try)
import qualified RIO.Text as Text
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map.Partial

import Text.Megaparsec
    ( parse, errorBundlePretty, sepBy1, sepEndBy1, Parsec, ParseErrorBundle, try, anySingle, takeWhile1P, choice, lookAhead )
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

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

integer :: Parser Int
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

message :: Parser Text
message = takeWhile1P Nothing (`elem` ['a', 'b'])

ruleToParser :: Rules -> Rule -> Parser Text
ruleToParser _ (CharRule c) = Text.singleton <$> char c
ruleToParser rs (SubRule r) = chcRules r
    where seqRules xs = Text.concat <$> mapM (ruleToParser rs . (rs Map.Partial.!)) xs
          chcRules ys = choice (map (try . seqRules) ys)

ruleToParser' :: Rules -> [Rule] -> Text -> Parser Text
ruleToParser' _  []              pre = return pre
ruleToParser' rs (CharRule c:ps) pre = do
    pre' <- Text.snoc pre <$> char c
    ruleToParser' rs ps pre'
ruleToParser' rs (SubRule r:ps)  pre = choice (map seqRules r)
    where seqRules :: [Int] -> Parser Text
          seqRules xs = try $ ruleToParser' rs (map (rs Map.Partial.!) xs <> ps) pre

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => FilePath -> Parser a -> m a
readInput file p = do
    x <- parse p file <$> readFileUtf8 file
    either (\e -> do { logError $ display e; exitFailure })
           return x

completeLine :: Parser a -> Parser a
completeLine p = do
    x <- p
    lookAhead eol
    return x

runA :: (HasLogFunc env) => RIO env ()
runA = do
    m <- readInput "data/day19.txt" $ do
        r <- rules
        eol
        let p = completeLine $ ruleToParser r (r Map.Partial.! 0)
        y <- (try (Just <$> p) <|> (Nothing <$ message)) `sepEndBy1` eol
        return $ catMaybes y
    logInfo $ display $ length m

runB :: (HasLogFunc env) => RIO env ()
runB =  do
    m <- readInput "data/day19-alt.txt" $ do
        r <- rules
        eol
        let p = completeLine $ ruleToParser' r [r Map.Partial.! 0] ""
        y <- (try (Just <$> p) <|> (Nothing <$ message)) `sepEndBy1` eol
        return $ catMaybes y
    logInfo $ display $ length m
