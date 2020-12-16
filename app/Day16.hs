{-# LANGUAGE TemplateHaskell #-}
module Day16 where

import RIO
import RIO.State
import RIO.List.Partial (head)
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Map as Map
import Lens.Micro.Platform ( (.=), use, makeLenses )

import Text.Megaparsec
    ( parse,
      errorBundlePretty,
      sepBy1,
      sepEndBy1,
      Parsec,
      MonadParsec(takeWhile1P),
      ParseErrorBundle )
import Text.Megaparsec.Char ( char, string, eol )
import Text.Megaparsec.Char.Lexer ( decimal )

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

type Range = (Int, Int)
type TicketRange = (Range, Range)
type Ticket = [Int]

data TicketData = TicketData
    { ticketRanges :: Map Text TicketRange
    , yourTicket :: Ticket
    , nearbyTickets :: [Ticket]
    } deriving (Show)

range :: Parser Range
range = do
    x <- decimal
    _ <- char '-'
    y <- decimal
    return (x, y)

ticketRange :: Parser (Text, TicketRange)
ticketRange = do
    name <- takeWhile1P Nothing (not . (`elem` ['\n', ':']))
    _ <- string ": "
    a <- range
    _ <- string " or "
    b <- range
    return (name, (a, b))

ticketRangeMap :: Parser (Map Text TicketRange)
ticketRangeMap = Map.fromList <$> ticketRange `sepEndBy1` eol

intList :: Parser [Int]
intList = decimal `sepBy1` char ','

ticketData :: Parser TicketData
ticketData = do
    ticketRanges <- ticketRangeMap
    eol >> string "your ticket:" >> eol
    yourTicket <- intList
    eol >> eol >> string "nearby tickets:" >> eol
    nearbyTickets <- intList `sepEndBy1` eol
    return TicketData{..}

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => m TicketData
readInput = do
    x <- parse ticketData "data/day16.txt" <$> readFileUtf8 "data/day16.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

inRange :: Int -> Range -> Bool
inRange n (a, b) = a <= n && n <= b

inTicketRange :: Int -> TicketRange -> Bool
inTicketRange n (r1, r2) = inRange n r1 || inRange n r2

runA :: (HasLogFunc env) => RIO env ()
runA = do
    TicketData{..} <- readInput
    let error_rate = sum $ filter (\n -> not $ any (inTicketRange n) (Map.elems ticketRanges)) 
                                  (concat nearbyTickets)
    logInfo $ display error_rate

data MatchState = MatchState
    { _msRanges :: Map Text TicketRange
    , _msTickets :: [[Int]] }

makeLenses ''MatchState

matchSingleProp :: MonadState MatchState m => [Int] -> m (Either [Int] (Text, [Int]))
matchSingleProp nums = do
    ranges <- use msRanges
    let (m, n) = List.partition (\(_ ,v) -> all (`inTicketRange` v) nums)
                                (Map.toList ranges)
    case m of
        [(k,_)] -> do msRanges .= Map.fromList n
                      return $ Right (k, nums)
        _       -> return $ Left nums

matchProps :: MonadState MatchState m => m [(Text, [Int])]
matchProps = do
    nums <- use msTickets
    lst <- mapM matchSingleProp nums
    msTickets .= lefts lst
    return $ rights lst

matchSingleTicket :: MonadState MatchState m 
                  => (Text, TicketRange) -> m (Either (Text, TicketRange) (Text, [Int]))
matchSingleTicket (k, v) = do
    nums <- use msTickets
    let (m, n) = List.partition (all (`inTicketRange` v)) nums
    case m of
        [a] -> do msTickets .= n
                  return $ Right (k, a)
        _   -> return $ Left (k, v)

matchTickets :: MonadState MatchState m => m [(Text, [Int])]
matchTickets = do
    lst <- mapM matchSingleTicket =<< (Map.toList <$> use msRanges)
    msRanges .= Map.fromList (lefts lst)
    return $ rights lst

matchDone :: MonadState MatchState m => m Bool
matchDone = null <$> use msTickets

matchAllProps :: Map Text TicketRange -> [[Int]] -> Map Text [Int]
matchAllProps r t = Map.fromList $ evalState go $ MatchState r t
    where go = do
            p <- matchDone
            if p then return []
            else do
                a <- matchProps
                b <- matchTickets
                c <- go
                return $ concat [a, b, c]

runB :: (HasLogFunc env) => RIO env ()
runB = do
    TicketData{..} <- readInput
    let valid_tickets = filter (all (\n -> any (inTicketRange n) (Map.elems ticketRanges)))
                               nearbyTickets
        prop_nums = List.transpose (yourTicket : valid_tickets)
        matched = matchAllProps ticketRanges prop_nums
        departure = Map.filterWithKey (\k _ -> "departure" `Text.isPrefixOf` k) matched
        result = foldl' (*) 1 $ map head $ Map.elems departure
    logInfo $ display result
