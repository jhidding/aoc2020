{-# LANGUAGE QuasiQuotes #-}
module Day11 where

import RIO
import qualified RIO.List as List
import RIO.List.Partial (head)
import qualified RIO.Text as Text
import qualified Data.Array.Repa as Repa
import Data.Array.Repa ((!), (:.)(..), Z(..), U(..), DIM2)
import Data.Array.Repa.Stencil.Dim2 (stencil2, makeStencil2, mapStencil2)
import Data.Array.Repa.Stencil (Boundary(..))

type Room = Repa.Array U DIM2 Int

readInput :: MonadIO m => m Room
readInput = process . toLst <$> readFileUtf8 "data/day11.txt"
    where process lst = Repa.fromListUnboxed (shape lst) $ map toInt $ concat lst
          toLst = lines . Text.unpack
          toInt c | c == '.' = 0
                  | c == 'L' = 1
          shape lst = Z :. length lst :. length (head lst) :: DIM2

printRoom :: Room -> Text
printRoom room = Text.unlines (map Text.pack (split width (map toChar (Repa.toList room))))
    where toChar i | i == 0 = '.'
                   | i == 1 = 'L'
                   | i == 10 = '#'
                   | otherwise = 'E'
          width = head $ Repa.listOfShape $ Repa.extent room
          split _ [] = []
          split n lst = init : split n rest
                      where (init, rest) = List.splitAt n lst

step :: Monad m => Room -> m Room
step = Repa.computeUnboxedP . nextGen
    where nextGen room = Repa.zipWith comp room $ mapStencil2 (BoundConst 0) neighbours room
          comp :: Int -> Int -> Int
          comp j i | j == 0             = 0
                   | j == 1  && i <  10 = 10
                   | j == 10 && i >= 40 = 1
                   | otherwise          = j
          neighbours = [stencil2| 1 1 1
                                  1 0 1
                                  1 1 1 |]

fixedPointM :: (Monad m) => (Room -> m Room) -> Room -> m Room
fixedPointM f x = do
    y <- f x
    eq <- Repa.equalsP x y
    if eq then return x else fixedPointM f y

countOccupied :: (Monad m) => Room -> m Int
countOccupied room = do
    Repa.sumAllP $ Repa.map (\x -> if x == 10 then 1 else 0) room

directions :: [DIM2]
directions =
    [ Z :. -1 :. -1
    , Z :. -1 :.  0
    , Z :. -1 :.  1
    , Z :.  0 :. -1
    , Z :.  0 :.  1
    , Z :.  1 :. -1
    , Z :.  1 :.  0
    , Z :.  1 :.  1 ]

scanLineOfSight :: Room -> DIM2 -> DIM2 -> Int
scanLineOfSight room p d
    | not inRange = 0
    | here  == 0  = scanLineOfSight room p' d
    | otherwise   = here
    where inRange = Repa.inShape (Repa.extent room) p'
          here = room ! p'
          p' = Repa.addDim p d

pointSum :: Room -> Int -> Int
pointSum room flatIdx = sum $ map (scanLineOfSight room p) directions
    where p = Repa.fromIndex (Repa.extent room) flatIdx

indices :: Room -> Room
indices room = Repa.fromListUnboxed shape [0..size-1]
    where shape = Repa.extent room
          size = Repa.size shape

rules2 :: Int -> Int -> Int
rules2 j i | j == 1  && i <  10  = 10
           | j == 10 && i >= 50  = 1
           | otherwise           = j

step2 :: Monad m => Room -> Room -> m Room
step2 idxArr room = Repa.computeUnboxedP next
    where next = Repa.zipWith rules2 room visible
          visible = Repa.map (pointSum room) idxArr

runA :: (HasLogFunc env) => RIO env ()
runA = do
    occ <- readInput >>= fixedPointM step >>= countOccupied
    logInfo $ display occ

runB :: (HasLogFunc env) => RIO env ()
runB = do
    room <- readInput
    occ <- fixedPointM (step2 (indices room)) room >>= countOccupied
    logInfo $ display occ
