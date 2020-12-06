module Day06 where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.List.Partial as List.Partial
import qualified RIO.Set as Set

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy sep = List.unfoldr (break' (== sep) . List.dropPrefix [sep])
    where break' _    []  = Nothing
          break' pred lst = Just $ List.break pred lst

readData :: MonadIO m => m [[Text]]
readData = splitBy "" . Text.lines <$> readFileUtf8 "data/day06.txt"

runA :: (HasLogFunc env) => RIO env ()
runA = do
    counts <- map (length . List.nub . Text.unpack . Text.concat) <$> readData
    logInfo $ display $ sum counts

runB :: (HasLogFunc env) => RIO env ()
runB = do
    counts <- map (Set.size . common) <$> readData
    logInfo $ display $ sum counts 
    where common = List.Partial.foldl1' Set.intersection . map toSet
          toSet = Set.fromList . Text.unpack