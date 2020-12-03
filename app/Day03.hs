module Day03 where

import RIO
import Data.Ratio ( denominator, numerator, (%) )
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Vector.Unboxed as UVector
import qualified RIO.Vector.Unboxed.Partial as UVector.Partial

type TreeMap = [UVector.Vector Bool]

readData :: MonadIO m => m TreeMap
readData = List.cycle . map convertLine . transposeLines <$> readFileUtf8 "data/day03.txt"
    where convertLine line = UVector.fromList (map (== '#') line)
          transposeLines text = List.transpose (map Text.unpack $ Text.lines text)

countTrees :: TreeMap -> Rational -> Int
countTrees trees frac = countTrees' trees 0 0
    where countTrees' (row : trees) loc cnt
            | loc >= fromIntegral (UVector.length row)  = cnt
            | otherwise                  = countTrees' trees (loc + frac) (cnt + isTree row loc)
          isTree row loc
            | denominator loc == 1       = if row UVector.Partial.! fromIntegral (numerator loc)
                                           then 1 else 0
            | otherwise                  = 0

runA :: (HasLogFunc env) => RIO env ()
runA = do
    treeMap <- readData
    logInfo $ display $ countTrees treeMap (1 % 3)

runB :: (HasLogFunc env) => RIO env ()
runB = do
    treeMap <- readData
    logInfo $ display $ foldl' (*) 1 (map (countTrees treeMap) slopes)
    where slopes = [1 % 1, 1 % 3, 1 % 5, 1 % 7, 2 % 1]
