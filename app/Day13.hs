{-# LANGUAGE TupleSections #-}
module Day13 where

import RIO hiding (try)
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.List.Partial as List.Partial
import Text.Megaparsec
import Text.Megaparsec.Char ( char, eol )
import Text.Megaparsec.Char.Lexer ( decimal )

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

inputP :: Parser (Int, [Maybe Int])
inputP = do
    x <- decimal
    eol
    y <- (Just <$> try decimal <|> Nothing <$ char 'x') `sepBy` char ','
    eol
    return (x, y)

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => m (Int, [Maybe Int])
readInput = do
    x <- parse inputP "data/day13.txt" <$> readFileUtf8 "data/day13.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

earliestBus :: Int -> [Int] -> (Int, Int)
earliestBus t busses = List.Partial.minimumBy (compare `on` snd) nextTimes
    where nextTimes = map nextTime busses
          nextTime n = (n, n - t `mod` n)

findTimestamp :: Integer -> Integer -> [(Integer, Integer)] -> Integer
findTimestamp start _    [] = start
findTimestamp start step ((d, f) : rest) = findTimestamp hit (step * f) rest
        where fit t = (t + d) `mod` f == 0
              hit = List.Partial.head $ filter fit [start, start+step ..]

runA :: (HasLogFunc env) => RIO env ()
runA = do
    (t, busses) <- readInput
    let (b, t') = earliestBus t (catMaybes busses)
    logInfo $ display $ tshow (t' * b)

runB :: (HasLogFunc env) => RIO env ()
runB = do
    (_, busses) <- readInput
    let delays = catMaybes $ zipWith (\a b -> (a,) <$> b) 
                             [0..] (map (fmap fromIntegral) busses)
    logInfo $ display $ tshow $ findTimestamp 0 1 delays
