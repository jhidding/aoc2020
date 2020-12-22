{-# LANGUAGE TupleSections #-}
module Day20 where

import RIO hiding (try)
import RIO.State

import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Map.Partial as Map.Partial

import RIO.List.Partial (head)
import Data.Tuple (swap)
import Lens.Micro.Platform ( _1, _2, (.=), use, at, (%=) )

import qualified Text.Megaparsec.Char.Lexer as L
import Parsing ( sepEndBy1, char, eol, string, Parser, readInput )
import qualified Data.Massiv.Array as A

type Grid2 = A.Array A.U A.Ix2 Int
type Grid2' r = A.Array r A.Ix2 Int
type EdgeSlice = A.Array A.M A.Ix1 Int

tile :: Parser (Int, Grid2)
tile = do
    string "Tile "
    n <- L.decimal
    string ":\n"
    l <- some ((char '.' $> 0) <|> (char '#' $> 1)) `sepEndBy1` eol
    case A.fromListsM A.Seq l of
        Nothing -> fail $ "input not a grid: " <> show l
        Just grid' -> return (n, grid')

type TileMap = Map Int Grid2
data Edge = Edge Int Int deriving (Show, Eq, Ord)

allEdges :: TileMap -> [Edge]
allEdges tm = Edge <$> Map.keys tm <*> [0..3]

edgeData :: TileMap -> Edge -> EdgeSlice
edgeData tm (Edge n i) = edgeData' i d
    where d = tm Map.Partial.! n

edgeData' :: Int -> Grid2 -> EdgeSlice
edgeData' i d
    | i == 0    = d A.!> 0
    | i == 1    = d A.<! 9
    | i == 2    = d A.!> 9
    | i == 3    = d A.<! 0

matchEdge :: TileMap -> Edge -> Edge -> Bool
matchEdge tm d1 d2 = not (sameTile d1 d2)
                  && (A.eqArrays (==) e1 e2 
                  ||  A.eqArrays (==) e1 (A.reverse A.Dim1 e2))
    where e1 = edgeData tm d1
          e2 = edgeData tm d2
          sameTile (Edge i _) (Edge j _) = i == j

countMatches :: TileMap -> Int -> Int
countMatches tm i = length $ filter (\e -> any (matchEdge tm e) (allEdges tm)) myEdges
    where myEdges = Edge i <$> [0..3]

uniqueMatches :: TileMap -> Int -> Int
uniqueMatches tm i = length $ filter unique myEdges
    where myEdges = Edge i <$> [0..3]
          unique e = length (filter (matchEdge tm e) (allEdges tm)) == 1

type Orientation = Int    -- 0..7  0..3 - position of top edge, 4..7 - mirrors

data Tessellation = Tessellation
    { _tileMap :: TileMap
    , _tileGrid :: Map (Int, Int) Grid2
    } deriving (Show)

tileMap :: Lens' Tessellation TileMap
tileMap = lens _tileMap (\t x -> t { _tileMap = x })
tileGrid :: Lens' Tessellation (Map (Int, Int) Grid2)
tileGrid = lens _tileGrid (\t x -> t { _tileGrid = x })

orientGrid :: Orientation -> Grid2 -> Grid2' A.D
orientGrid o
    | o == 0 = A.delay
    | o == 1 = A.reverse A.Dim2 . A.transpose
    | o == 2 = A.reverse A.Dim1 . A.reverse A.Dim2
    | o == 3 = A.reverse A.Dim1 . A.transpose
    | o == 4 = A.reverse A.Dim2
    | o == 5 = A.transpose . A.reverse A.Dim1 . A.reverse A.Dim2
    | o == 6 = A.reverse A.Dim1
    | o == 7 = A.transpose

matchEdge' :: Edge' -> Edge' -> Maybe Int
matchEdge' (Edge' o1 e1) (Edge' o2 e2)
    | A.eqArrays (==) e1 e2 = Just getOrientation
    | A.eqArrays (==) e1 (A.reverse A.Dim1 e2) = Just getRevOrientation
    | otherwise = Nothing
    where getOrientation = oMatrix A.! (o1 A.:. o2)
          getRevOrientation = (getOrientation + 4) `mod` 8
          oMatrix :: A.Array A.U A.Ix2 Int
          oMatrix = A.fromLists' A.Seq
            [[6, 5, 0, 3], [7, 4, 1, 0], [0, 3, 6, 5], [1, 0, 7, 4]]

placeTile :: MonadState Tessellation m => Int -> Orientation -> (Int, Int) -> m ()
placeTile i o p = do
    tile <- use (tileMap . at i)
    tileGrid . at p .= (A.compute . orientGrid o <$> tile)
    tileMap %= Map.delete i

data Edge' = Edge' Int EdgeSlice

neighbour :: (Int, Int) -> Int -> (Int, Int)
neighbour (x, y) o
    | o == 0 = (x, y-1)
    | o == 1 = (x+1, y)
    | o == 2 = (x, y+1)
    | o == 3 = (x-1, y)

getGridEdge :: MonadState Tessellation m => (Int, Int) -> Int -> m (Maybe Edge')
getGridEdge p o = do
    g <- use (tileGrid . at p)
    return $ Edge' o <$> (edgeData' o <$> g)

freeEdges :: Map k Grid2 -> [(k, Edge')]
freeEdges m =
    concatMap (\(i, g) -> map (\o -> (i, Edge' o (edgeData' o g))) [0..3])
              (Map.toList m)

singleMatch :: [a] -> Maybe a
singleMatch [x] = Just x
singleMatch _   = Nothing

step :: MonadState Tessellation m => m ()
step = do
    tileEdges <- use (tileMap . to freeEdges)
    gm <- use tileGrid
    let trEdge (p, e@(Edge' o _)) = (neighbour p o, e)
        gridEdges = filter (\(p, _) -> p `Map.notMember` gm) (map trEdge $ freeEdges gm)
        fm ge = singleMatch $ mapMaybe (\(i, te) -> (i,) <$> matchEdge' ge te) tileEdges
        sols = List.nub $ mapMaybe (\(p, ge) -> (p,) <$> fm ge) gridEdges
        place (p, (i, o)) = placeTile i o p
    traceM (tshow sols)
    mapM_ place sols

solve :: MonadState Tessellation m => m (Map (Int, Int) Grid2)
solve = do
    done <- use (tileMap . to Map.null)
    if done then use tileGrid else step >> solve

firstStep :: MonadState Tessellation m => m ()
firstStep = do
    first <- use (tileMap . to Map.keys . to head)
    placeTile first 0 (0, 0)

runA :: (HasLogFunc env) => RIO env ()
runA = do
    tm <- readInput "data/day20.txt" (Map.fromList <$> tile `sepEndBy1` eol)
    let cs = zip (Map.keys tm) $ map (countMatches tm) (Map.keys tm)
        cs2 = filter ((== 2) . snd) cs
        result = foldl' (*) 1 (map fst cs2)
    logInfo $ display $ tshow result

runB :: (HasLogFunc env) => RIO env ()
runB = do
    tm <- readInput "data/day20.txt" (Map.fromList <$> tile `sepEndBy1` eol)
    let grid = evalState (firstStep >> solve) (Tessellation tm Map.empty)
    logInfo $ display $ tshow grid
