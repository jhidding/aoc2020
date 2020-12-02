module Day01 where

import RIO
import Control.Applicative ( Alternative(empty) )
-- import RIO.Prelude.Types (Alternative)
import qualified RIO.Text as Text
import qualified RIO.Set as Set

altMap :: (Alternative p, Foldable f) => (a -> p b) -> f a -> p b
altMap f = foldr ((<|>) . f) empty

readInput :: MonadIO m => m [Int]
readInput = do
    text <- Text.lines <$> readFileUtf8 "data/day01a.txt"
    return $ mapMaybe (readMaybe . Text.unpack) text

searchNSum :: Int -> Int -> Set Int -> Maybe [Int]
searchNSum n total numbers
    | total < 0   = Nothing
    | n <= 0      = Nothing
    | n == 1      = if total `Set.member` numbers
                    then Just [total] else Nothing
    | otherwise   = altMap (\m -> (<> [m]) <$> searchNSum (n - 1) (total - m) numbers) numbers

runA :: (HasLogFunc env, MonadReader env m, MonadIO m) => m ()
runA = do
    answer <- searchNSum 2 2020 . Set.fromList <$> readInput
    case answer of
        Nothing -> logInfo "No answer found"
        Just x  -> logInfo $ display $ foldl' (*) 1 x

runB :: (HasLogFunc env, MonadReader env m, MonadIO m) => m ()
runB = do
    answer <- searchNSum 3 2020 . Set.fromList <$> readInput
    case answer of
        Nothing -> logInfo "No answer found"
        Just x  -> logInfo $ display $ foldl' (*) 1 x