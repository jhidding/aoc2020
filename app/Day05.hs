module Day05 where

import RIO
import RIO.List (sort)
import qualified RIO.Text as Text

readData :: MonadIO m => m [Text]
readData = Text.lines <$> readFileUtf8 "data/day05.txt"

codeToSeats :: Text -> (Int, Int)
codeToSeats t = ( codeToNum 'F' 'B' (Text.unpack firstPart) 0
                , codeToNum 'L' 'R' (Text.unpack secondPart) 0)
    where codeToNum :: Char -> Char -> [Char] -> Int -> Int
          codeToNum _ _ [] y = y
          codeToNum bit0 bit1 (x:xs) y
            | x == bit0 = codeToNum bit0 bit1 xs (2*y)
            | x == bit1 = codeToNum bit0 bit1 xs (2*y + 1)
          (firstPart, secondPart) = Text.span (\c -> c == 'F' || c == 'B') t

seatId :: (Int, Int) -> Int
seatId (row, column) = 8*row + column

findSeat :: [Int] -> Maybe Int
findSeat (a:b:xs)
    | b - a == 2  = Just $ a + 1
    | otherwise   = findSeat (b:xs)
findSeat _ = Nothing

runA :: (HasLogFunc env) => RIO env ()
runA = do
    seatIds <- map (seatId . codeToSeats) <$> readData
    logInfo $ display $ foldl' max 0 seatIds

runB :: (HasLogFunc env) => RIO env ()
runB = do
    seatIds <- sort . map (seatId . codeToSeats) <$> readData
    logInfo $ display $ tshow $ findSeat seatIds
