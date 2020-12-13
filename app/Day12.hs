{-# LANGUAGE TemplateHaskell #-}
module Day12 where

import RIO hiding (try)
import Lens.Micro.Platform
    ( (+~), (%=), (+=), use, makeLenses, _1, _2 )

import qualified RIO.Text as Text
import RIO.State
    ( MonadState, State, gets, modify, evalState )

import Text.Megaparsec
    ( parse,
      errorBundlePretty,
      sepEndBy1,
      Parsec,
      MonadParsec(try),
      ParseErrorBundle )
import Text.Megaparsec.Char ( char, eol )
import Text.Megaparsec.Char.Lexer ( decimal )

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

readInput :: (HasLogFunc env, MonadReader env m, MonadIO m, Turtle t) => m [t ()]
readInput = do
    program <- parse (singleInstruction `sepEndBy1` eol) "data/day12.txt"
                <$> readFileUtf8 "data/day12.txt"
    either (\e -> do { logError $ display e; return [] })
           return program

data ManhattanRec = ManhattanRec
    { _mdDirection :: Int
    , _mdDist :: (Int, Int) }

makeLenses ''ManhattanRec

newtype ManhattanDistance a = ManhattanDistance
    { runMD :: State ManhattanRec a }
    deriving (Functor, Applicative, Monad, MonadState ManhattanRec)

instance Turtle ManhattanDistance where
    goNorth = (mdDist . _1 +=)
    goEast = (mdDist . _2 +=)
    turnLeft x = mdDirection %= \d -> (d + x) `mod` 360
    moveForward x = use mdDirection >>= go
            where go dir | dir == 0   = goEast x
                         | dir == 90  = goNorth x
                         | dir == 180 = goWest x
                         | dir == 270 = goSouth x
                         | otherwise  = return ()
    getManhattanDistance = gets go
        where go m = abs (m ^. mdDist . _1) + abs (m ^. mdDist . _2)

data WaypointRec = WaypointRec
    { _waypointLoc  :: (Int, Int)
    , _waypointDist :: (Int, Int) }

makeLenses ''WaypointRec

newtype Waypoint a = Waypoint
    { runWaypoint :: State WaypointRec a }
    deriving (Functor, Applicative, Monad, MonadState WaypointRec)

rotate :: Int -> (Int, Int) -> (Int, Int)
rotate a (x, y)
    | a == 90   = (-y,  x)
    | a == 180  = (-x, -y)
    | a == 270  = ( y, -x)
    | otherwise = ( x,  y)

instance Turtle Waypoint where
    goEast = (waypointLoc . _1 +=)
    goNorth = (waypointLoc . _2 +=)
    turnLeft x = waypointLoc %= rotate (x `mod` 360)
    moveForward x = modify go
        where go m = m & (waypointDist . _1) +~ (x * (m ^. waypointLoc . _1))
                       & (waypointDist . _2) +~ (x * (m ^. waypointLoc . _2))
    getManhattanDistance = gets go
        where go m = abs (m ^. waypointDist . _1) + abs (m ^. waypointDist . _2)

runA :: (HasLogFunc env) => RIO env ()
runA = do
    program <- readInput
    let result = evalState (runMD $ sequence_ program >> getManhattanDistance)
                           (ManhattanRec 0 (0, 0))
    logInfo $ display $ tshow result

runB :: (HasLogFunc env) => RIO env ()
runB = do
    program <- readInput
    let result = evalState (runWaypoint $ sequence_ program >> getManhattanDistance)
                           (WaypointRec (10, 1) (0, 0))
    logInfo $ display $ tshow result
