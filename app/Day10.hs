module Day10 where

import RIO
import qualified RIO.List as List
import RIO.List.Partial (head)
import qualified RIO.Text as Text

readInput :: MonadIO m => m [Int]
readInput = do
    text <- Text.lines <$> readFileUtf8 "data/day10.txt"
    return $ mapMaybe (readMaybe . Text.unpack) text

joltDiffs :: [Int] -> [Int]
joltDiffs (x1:x2:xs) = (x2 - x1) : joltDiffs (x2:xs)
joltDiffs _          = []

count :: (Eq a) => a -> [a] -> Int
count y = length . filter (== y)

runA :: (HasLogFunc env) => RIO env ()
runA = do
    jolts <- (0 :) . List.sort <$> readInput
    let diffs = joltDiffs jolts
        ones = count 1 diffs
        threes = count 3 diffs
    logInfo $ display $ tshow (ones, threes, ones * (threes+1))

groupSeq :: Eq a => [a] -> [(a, Int)]
groupSeq = map (\x -> (head x, length x)) . List.group

runB :: (HasLogFunc env) => RIO env ()
runB = do
    diffs <- groupSeq . joltDiffs . (0 :) . List.sort <$> readInput
    let oneSeqs = map (nPaths . snd) $ filter (\(x, _) -> x == 1) diffs
        -- last bit needs to be used: generic 2^(n-1); in bit pattern
        -- no skips larger than two zeros are allowed. My input does'nt
        -- contain sequences longer than four ones.
        nPaths :: Int -> Integer
        nPaths n
            | n == 1 = 1
            | n == 2 = 2
            | n == 3 = 4
            | n == 4 = 7
            | otherwise = 0
    logInfo $ display $ tshow (foldl' (*) 1 oneSeqs)
