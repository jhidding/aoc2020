module Day15 where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO.List.Partial (head)

import Text.Megaparsec
    ( parse, errorBundlePretty, sepBy1, Parsec, ParseErrorBundle )
import Text.Megaparsec.Char ( char )
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => m [Int]
readInput = do
    x <- parse (decimal `sepBy1` char ',' :: Parser [Int]) 
               "data/day15.txt" <$> readFileUtf8 "data/day15.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

-- this one's not so good O(n^2)?
nextNumber :: [Int] -> [Int]
nextNumber (x:xs) = maybe 0 (+ 1) idx : x : xs
    where idx = List.findIndex (== x) xs

nThNumber :: [Int] -> Int -> Int -> Int
nThNumber nums turn n
    | n == turn = head nums
    | otherwise = nThNumber (nextNumber nums) (turn + 1) n

-- this is still not good enough, map every turn makes this O(n^2)
nextNumber' :: (Map Int Int, Int) -> (Map Int Int, Int)
nextNumber' (m, i) = (Map.insert i 1 (Map.map (+1) m), fromMaybe 0 (m Map.!? i))

nThNumber' :: [Int] -> Int -> Int -> Int
nThNumber' nums turn n = go (turn+1) (Map.fromList (zip nums [1..]), 0)
    where go turn (m, i)
            | turn == n = i
            | otherwise = go (turn+1) $ nextNumber' (m, i)

-- one lookup and one insert every turn makes this O(n log n)
-- this still takes half a minute to run
data Turn = Turn
    { memory :: Map Int Int
    , turn :: Int
    , last :: Int } deriving (Show)

nextNumber'' :: Turn -> Turn
nextNumber'' Turn{..} = Turn
    { memory = Map.insert last (- turn) memory
    , turn = turn + 1
    , last = maybe 0 (+ turn) (memory Map.!? last) }

nThNumber'' :: [Int] -> Int -> Int -> Int
nThNumber'' nums turn' n = go Turn { memory = Map.fromList (zip nums [-turn'..])
                                   , turn = turn' + 1, last = 0 }
    where go t
            | turn t == n = last t
            | otherwise   = go $ nextNumber'' t

runA :: (HasLogFunc env) => RIO env ()
runA = do
    x <- readInput
    logInfo $ display $ tshow $ nThNumber (reverse x) (length x) 2020

runB :: (HasLogFunc env) => RIO env ()
runB =  do
    x <- readInput
    logInfo $ display $ tshow $ nThNumber'' (reverse x) (length x) 30000000
