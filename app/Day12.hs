module Day12 where

import RIO hiding (try)
import qualified RIO.Text as Text
import RIO.State

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

class (Monad m) => Turtle m where
    goNorth :: Int -> m ()
    goNorth = goSouth . negate
    goSouth :: Int -> m ()
    goSouth = goNorth . negate
    goEast :: Int -> m ()
    goEast = goWest . negate
    goWest :: Int -> m ()
    goWest = goEast . negate
    turnLeft :: Int -> m ()
    turnLeft = turnRight . negate
    turnRight :: Int -> m ()
    turnRight = turnLeft . negate
    moveForward :: Int -> m ()
    getManhattanDistance :: m Int

singleInstruction :: (Turtle m) => Parser (m ())
singleInstruction
    =   try (goEast      <$> (char 'E' >> decimal))
    <|> try (goWest      <$> (char 'W' >> decimal))
    <|> try (goNorth     <$> (char 'N' >> decimal))
    <|> try (goSouth     <$> (char 'S' >> decimal))
    <|> try (turnLeft    <$> (char 'L' >> decimal))
    <|> try (turnRight   <$> (char 'R' >> decimal))
    <|> try (moveForward <$> (char 'F' >> decimal))

data ManhattanRec = ManhattanRec
    { mdDirection :: Int
    , mdEastWest :: Int
    , mdNorthSouth :: Int }

newtype ManhattanDistance a = ManhattanDistance
    { runMD :: State ManhattanRec a }
    deriving (Functor, Applicative, Monad, MonadState ManhattanRec)

instance Turtle ManhattanDistance where
    goNorth     x = modify (\m@ManhattanRec{..} -> m { mdNorthSouth = mdNorthSouth + x })
    goEast      x = modify (\m@ManhattanRec{..} -> m { mdEastWest   = mdEastWest + x })
    turnLeft    x = modify (\m@ManhattanRec{..} -> m { mdDirection  = (mdDirection + x) `mod` 360 })
    moveForward x = gets mdDirection >>= go
            where go dir | dir == 0   = goEast x
                         | dir == 90  = goNorth x
                         | dir == 180 = goWest x
                         | dir == 270 = goSouth x
                         | otherwise  = return ()
    getManhattanDistance = gets go
        where go ManhattanRec{..} = abs mdEastWest + abs mdNorthSouth

data WaypointRec = WaypointRec
    { waypointX :: Int
    , waypointY :: Int
    , waypointEast :: Int
    , waypointNorth :: Int }

newtype Waypoint a = Waypoint
    { runWaypoint :: State WaypointRec a }
    deriving (Functor, Applicative, Monad, MonadState WaypointRec)

instance Turtle Waypoint where
    goNorth        x = modify (\m@WaypointRec{..} -> m { waypointY = waypointY + x })
    goEast         x = modify (\m@WaypointRec{..} -> m { waypointX = waypointX + x })
    turnLeft       x = modify (go (x `mod` 360))
        where go x' m@WaypointRec{..}
                | x' == 90  = m { waypointX = -waypointY, waypointY = waypointX }
                | x' == 180 = m { waypointX = -waypointX, waypointY = -waypointY }
                | x' == 270 = m { waypointX =  waypointY, waypointY = -waypointX }
                | otherwise = m
    moveForward    x = modify (\m@WaypointRec{..} -> m 
                                { waypointEast = waypointEast + x * waypointX
                                , waypointNorth = waypointNorth + x * waypointY })
    getManhattanDistance = gets go
        where go WaypointRec{..} = abs waypointEast + abs waypointNorth

readInput :: (HasLogFunc env, MonadReader env m, MonadIO m, Turtle t) => m [t ()]
readInput = do
    program <- parse (singleInstruction `sepEndBy1` eol) "data/day12.txt"
                <$> readFileUtf8 "data/day12.txt"
    either (\e -> do { logError $ display e; return [] })
           return program

runA :: (HasLogFunc env) => RIO env ()
runA = do
    program <- readInput
    let result = evalState (runMD $ sequence_ program >> getManhattanDistance)
                           (ManhattanRec 0 0 0)
    logInfo $ display $ tshow result

runB :: (HasLogFunc env) => RIO env ()
runB = do
    program <- readInput
    let result = evalState (runWaypoint $ sequence_ program >> getManhattanDistance)
                           (WaypointRec 10 1 0 0)
    logInfo $ display $ tshow result
