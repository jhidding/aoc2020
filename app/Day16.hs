module Day16 where

import RIO
import RIO.State
import RIO.List.Partial (head)
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map.Partial
import Lens.Micro.Platform ( _1, _2, (.=), use )

import Text.Megaparsec
    ( MonadParsec(takeWhile1P) )
import Data.Tuple (swap)

import Parsing
    ( sepBy1, sepEndBy1, char, eol, string, Parser, readInput, integer, lexeme )

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
    x <- integer
    _ <- char '-'
    y <- integer
    return (x, y)

ticketRange :: Parser (Text, TicketRange)
ticketRange = do
    name <- takeWhile1P Nothing (not . (`elem` ['\n', ':']))
    _ <- lexeme $ string ":"
    a <- range
    _ <- lexeme $ string "or"
    b <- range
    return (name, (a, b))

ticketRangeMap :: Parser (Map Text TicketRange)
ticketRangeMap = Map.fromList <$> ticketRange `sepEndBy1` eol

intList :: Parser [Int]
intList = integer `sepBy1` char ','

ticketData :: Parser TicketData
ticketData = do
    ticketRanges <- ticketRangeMap
    eol >> string "your ticket:" >> eol
    yourTicket <- intList
    eol >> eol >> string "nearby tickets:" >> eol
    nearbyTickets <- intList `sepEndBy1` eol
    return TicketData{..}

inRange :: Int -> Range -> Bool
inRange n (a, b) = a <= n && n <= b

inTicketRange :: Int -> TicketRange -> Bool
inTicketRange n (r1, r2) = inRange n r1 || inRange n r2

runA :: (HasLogFunc env) => RIO env ()
runA = do
    TicketData{..} <- readInput "data/day16.txt" ticketData
    let error_rate = sum $ filter (\n -> not $ any (inTicketRange n) (Map.elems ticketRanges)) 
                                  (concat nearbyTickets)
    logInfo $ display error_rate

findMatch :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
findMatch pred as bs = evalState go (as, bs)
    where go = do p <- stop
                  if p then return []
                  else do i <- matchObvious pred
                          j <- map swap <$> swapped (matchObvious $ flip pred)
                          k <- go
                          return $ concat [i, j, k]

          -- Runs an action with the state tuple swapped
          swapped :: State (x, y) a -> State (y, x) a
          swapped action = do
              (x, s') <- runState action . swap <$> get
              put (swap s')
              return x

          -- If the computation is ended
          stop :: State ([a], [b]) Bool
          stop = use (_1 . to null)

          -- Match obvious pairs: takes from first list those elements that have only
          -- one match in the second list
          matchObvious :: (a -> b -> Bool) -> State ([a], [b]) [(a, b)]
          matchObvious pred = do
              lst <- mapM (matchSingle pred) =<< use _1
              _1 .= lefts lst
              return $ rights lst
          
          matchSingle :: (a -> b -> Bool) -> a -> State ([a], [b]) (Either a (a, b))
          matchSingle pred a = do
              (m, n) <- List.partition (pred a) <$> use _2
              case m of
                  [b] -> _2 .= n >> return (Right (a, b))
                  _   -> return $ Left a

runB :: (HasLogFunc env) => RIO env ()
runB = do
    TicketData{..} <- readInput "data/day16.txt" ticketData
    let valid_tickets = filter (all (\n -> any (inTicketRange n) (Map.elems ticketRanges)))
                               nearbyTickets
        -- transpose numbers
        prop_nums = List.transpose (yourTicket : valid_tickets)
        -- predicate that should hold for a property name and a list of values
        pred prop = all (\n -> inTicketRange n (ticketRanges Map.Partial.! prop))
        -- matched property names with values
        matched = findMatch pred (Map.keys ticketRanges) prop_nums
        -- fetch properties that start with "departure"
        departures = filter (\(k, _) -> "departure" `Text.isPrefixOf` k) matched
        -- compute product
        result = foldl' (*) 1 $ map (head . snd) departures

    logInfo $ display result
