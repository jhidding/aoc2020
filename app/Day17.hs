{-# LANGUAGE QuasiQuotes #-}
module Day17 where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO.List.Partial (head, tail, (!!))

import qualified Data.Massiv.Array as A
import Data.Massiv.Array (Padding(..), Sz(..), Border(..), idStencil, sumStencil)

type Grid2 = A.Array A.U A.Ix2 Int
type Grid3 = A.Array A.U A.Ix3 Int
type Grid4 = A.Array A.U A.Ix4 Int

readInput :: (MonadThrow m, MonadIO m) => m Grid2
readInput = readFileUtf8 "data/day17.txt" >>= toLsts >>= process
    where process = A.fromListsM A.Seq . map (map toInt)
          toLsts = return . lines . Text.unpack
          toInt c | c == '.' = 0
                  | c == '#' = 1
                  | otherwise = 255

printGrid :: Grid2 -> Text
printGrid room = Text.unlines (map (Text.pack . map toChar) (A.toLists room))
    where toChar i | i == 0 = '.'
                   | i == 1 = '#'
                   | otherwise = 'E'

expandDim :: MonadThrow m => Grid2 -> m Grid3
expandDim x = A.resizeM (A.consSz (A.Sz1 1) (A.size x)) x

padGrid :: Grid3 -> Grid3
padGrid = A.computeP . A.applyStencil (Padding (Sz3 1 1 1) (Sz3 1 1 1) (Fill 0)) idStencil

step :: Grid3 -> Grid3
step x = A.computeP $ A.zipWith lifeRules x y
    where y = A.computeAs A.U $ A.applyStencil (Padding (Sz3 1 1 1) (Sz3 1 1 1) (Fill 0))
                                               (sumStencil (Sz3 3 3 3)) x

expandDim4 :: MonadThrow m => Grid2 -> m Grid4
expandDim4 x = A.resizeM (A.consSz (A.Sz1 1) (A.consSz (A.Sz1 1) (A.size x))) x

padding4 :: Padding A.Ix4 Int
padding4 = Padding (Sz4 1 1 1 1) (Sz4 1 1 1 1) (Fill 0)

padGrid4 :: Grid4 -> Grid4
padGrid4 = A.computeP . A.applyStencil padding4 idStencil

step4 :: Grid4 -> Grid4
step4 x = A.computeP $ A.zipWith lifeRules x y
    where y = A.computeAs A.U $ A.applyStencil padding4
                                               (sumStencil (Sz4 3 3 3 3)) x

lifeRules :: Int -> Int -> Int
lifeRules j i | j == 1  && (i == 3 || i == 4) = 1
              | j == 0  &&  i == 3            = 1
              | otherwise                     = 0

runA :: (HasLogFunc env) => RIO env ()
runA = do
    grid <- readInput >>= expandDim
    let evolution = List.iterate (step . padGrid) grid
        sixthStep = evolution !! 6

    logInfo $ display $ A.sum sixthStep

runB :: (HasLogFunc env) => RIO env ()
runB =  do
    grid <- readInput >>= expandDim4
    let evolution = List.iterate (step4 . padGrid4) grid
        sixthStep = evolution !! 6

    logInfo $ display $ A.sum sixthStep
