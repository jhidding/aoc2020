module Day23 where

import RIO
import RIO.State
import RIO.List.Partial (head, maximum)

import Data.Vector.Unboxed ((!), thaw, freeze)
import Data.Vector.Unboxed.Mutable (IOVector, read, write)
import qualified RIO.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as MVector

class Monad m => Cups m where
    currentCup :: m Int
    take3 :: m (Int, Int, Int)
    findDestination :: (Int, Int, Int) -> m Int
    place3 :: (Int, Int, Int) -> Int -> m ()
    nextCup :: m ()

gameRound :: Cups m => m ()
gameRound = do
    (a, b, c) <- take3
    dest <- findDestination (a, b, c)
    place3 (a, b, c) dest
    nextCup

instance Cups (StateT [Int] Maybe) where
    currentCup = gets head

    take3 = do
        x <- gets (take 4)
        rest <- gets (drop 4)
        case x of
            [current,a,b,c] -> put (current:rest) >> return (a, b, c)

    findDestination (a, b, c) = do
        (current:rest) <- get
        let go i
                | i < 1 = go $ maximum rest
                | i `elem` [a, b, c] = go (i - 1)
                | otherwise = i
        return $ go (current - 1)

    place3 (a, b, c) dest = do
        (xs, y:ys) <- gets $ break (== dest)
        put $ xs <> (y:a:b:c:ys)

    nextCup = do
        (current:rest) <- get
        put $ rest <> [current]

fromOne :: Cups m => m ()
fromOne = do
    c <- currentCup
    if c == 1 then return () else nextCup >> fromOne

iterateN :: Monad m => Int -> m () -> m ()
iterateN n f
    | n == 0 = return ()
    | otherwise = f >> iterateN (n-1) f

instance Cups (RIO (IOVector Int)) where
    currentCup = do
        dat <- ask
        read dat 0
    
    take3 = do
        dat <- ask
        p1 <- read dat 0
        p2 <- read dat p1
        p3 <- read dat p2
        p4 <- read dat p3
        p5 <- read dat p4
        write dat p1 p5
        return (p2, p3, p4)
    
    findDestination (a, b, c) = do
        dat <- ask
        let go i
             | i < 1 = go (MVector.length dat - 1)
             | i `elem` [a, b, c] = go (i-1)
             | otherwise = i
        go . (+ (-1)) <$> read dat 0
    
    place3 (p2, _, p4) dest = do
        dat <- ask
        p1 <- read dat dest
        write dat dest p2
        write dat p4 p1
    
    nextCup = do
        dat <- ask
        p1 <- read dat 0
        p2 <- read dat p1
        write dat 0 p2

mulTwo :: RIO (IOVector Int) Int
mulTwo = do
    dat <- ask
    p1 <- read dat 1
    p2 <- read dat p1
    return $ p1*p2

toList' :: UVector Int -> Int -> [Int]
toList' vec start = a : toList' vec a
    where a = vec ! start

initialize1 :: [Int] -> RIO (IOVector Int) ()
initialize1 ys = do
    dat <- ask
    let go i (x:xs) = write dat i x >> go x xs
        go i [] = write dat i (head ys)
    go 0 ys

initialize2 :: [Int] -> RIO (IOVector Int) ()
initialize2 ys = do
    dat <- ask
    let go i (x:xs) = write dat i x >> go x xs
        go i [] = write dat i (length ys + 1)
    go 0 ys
    write dat (MVector.length dat - 1) (head ys)

runA :: (HasLogFunc env) => RIO env ()
runA = do
    v <- thaw (UVector.generate 10 id)
    x <- runRIO v (initialize1 [6, 1, 4, 7, 5, 2, 8, 3, 9]
                   >> iterateN 100 gameRound >> ask)
    v' <- freeze x
    logInfo $ display $ mconcat $ map tshow $ take 8 (toList' v' 1)

runB :: (HasLogFunc env) => RIO env ()
runB = do
    v <- thaw $ UVector.generate 1000001 (+1)
    x <- runRIO v (initialize2 [6, 1, 4, 7, 5, 2, 8, 3, 9]
                   >> iterateN 10000000 gameRound >> mulTwo)
    logInfo $ display $ tshow x
