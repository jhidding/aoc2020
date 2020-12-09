module Day09 where

import RIO
import Control.Applicative ( Alternative(empty) )
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Set as Set
import qualified RIO.Vector as Vector

altMap :: (Alternative p, Foldable f) => (a -> p b) -> f a -> p b
altMap f = foldr ((<|>) . f) empty

searchNSum :: Int -> Int -> Set Int -> Maybe [Int]
searchNSum n total numbers
    | total < 0   = Nothing
    | n <= 0      = Nothing
    | n == 1      = if total `Set.member` numbers
                    then Just [total] else Nothing
    | otherwise   = altMap (\m -> (<> [m]) <$> searchNSum (n - 1) (total - m) numbers) numbers

readInput :: MonadIO m => m [Int]
readInput = do
    text <- Text.lines <$> readFileUtf8 "data/day09.txt"
    return $ mapMaybe (readMaybe . Text.unpack) text

scanInput :: Int -> [Int] -> [(Set Int, Int)]
scanInput pn nums@(_:xs) = go preamble rest
    where go _        []     = []
          go preamble (y:_)  = (Set.fromList preamble, y) : scanInput pn xs
          (preamble, rest)   = List.splitAt pn nums

validate :: (Set Int, Int) -> Bool
validate (nums, n) = not $ null $ searchNSum 2 n nums

findSum :: Vector Int -> Int -> Maybe (Vector Int)
findSum nums n = findSum' 0 0 0 nums
    where findSum' a len total nums
            | total == n    = Just $ Vector.slice a len nums
            | total < n     = do v <- nums Vector.!? (a+len)
                                 findSum' a (len+1) (total+v) nums
            | total > n     = do v <- nums Vector.!? a
                                 findSum' (a+1) (len-1) (total-v) nums

runA :: (HasLogFunc env) => RIO env ()
runA = do
    msg <- readInput
    let result = snd <$> List.find (not . validate) (scanInput 25 msg)
    logInfo $ display $ tshow result

runB :: (HasLogFunc env) => RIO env ()
runB = do
    msg <- readInput
    let result = do
            y <- snd <$> List.find (not . validate) (scanInput 25 msg)
            z <- findSum (Vector.fromList msg) y
            a <- List.minimumMaybe z
            b <- List.maximumMaybe z
            return (a+b)
    logInfo $ display $ tshow result
