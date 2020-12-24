{-# LANGUAGE TupleSections #-}
module Day24 where

import RIO
import qualified RIO.List as List
import RIO.List.Partial ((!!))
import qualified RIO.Map as Map
import Coordinates (Coordinates(..))
import Parsing

directions :: Parser (Coordinates Int)
directions = sum <$> some
     (  string "e" $> Coordinates 1 0
    <|> string "w" $> Coordinates (-1) 0
    <|> string "ne" $> Coordinates 0 1
    <|> string "nw" $> Coordinates (-1) 1
    <|> string "se" $> Coordinates 1 (-1)
    <|> string "sw" $> Coordinates 0 (-1) )

neighbours :: Coordinates Int -> [Coordinates Int]
neighbours c = map (+ c)
    [ Coordinates 1 0
    , Coordinates 1 (-1)
    , Coordinates 0 (-1)
    , Coordinates (-1) 0
    , Coordinates (-1) 1
    , Coordinates 0 1 ]

type Floor = Map (Coordinates Int) Bool

flipTile :: Coordinates Int -> Floor -> Floor
flipTile = Map.alter (Just . maybe True not)

setDefault :: Coordinates Int -> Floor -> Floor
setDefault = Map.alter (<|> Just False)

paintDefaultRing :: Floor -> Floor
paintDefaultRing m = foldr setDefault m allNeighbours
    where allNeighbours = concatMap neighbours (Map.keys m)

stencil :: (Bool -> Int -> Maybe Bool) -> Floor -> Floor
stencil rules m = Map.mapMaybeWithKey (\p i -> rules i (stencilSum p)) m
    where stencilSum p = length $ filter (\p' -> Map.findWithDefault False p' m) (neighbours p)

rules :: Bool -> Int -> Maybe Bool
rules i j
    | j == 0           = Nothing     -- No neighbours, delete from floor
    | not i && j == 2  = Just True
    | i     && j > 2   = Just False
    | otherwise        = Just i

runA :: (HasLogFunc env) => RIO env ()
runA = do
    d <- readInput "data/day24.txt" (directions `sepEndBy1` eol)
    let result = length $ filter (odd . length) $ List.group $ List.sort d
    logInfo $ display result

runB :: (HasLogFunc env) => RIO env ()
runB = do
    d <- readInput "data/day24.txt" (directions `sepEndBy1` eol)
    let start = foldr flipTile Map.empty d
        gen = List.iterate (stencil rules . paintDefaultRing) start
        hundredth = gen !! 100
        result = length . filter id . Map.elems $ hundredth
    logInfo $ display result
