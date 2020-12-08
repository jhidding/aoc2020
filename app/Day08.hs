module Day08 where

import RIO
import qualified RIO.Vector as Vector
import qualified RIO.Set as Set
import Data.Vector.Generic.Mutable (write, read)

import Program
    ( Program, Machine(..), ProgramExitCode(..), Instruction(..)
    , runProgram, readProgram )

runA :: (HasLogFunc env) => RIO env ()
runA = do
    program <- readProgram "data/day08.txt"
    logInfo $ display $ tshow $ runProgram (Machine 0 0 program Set.empty)

modifiedPrograms :: Program -> [Program]
modifiedPrograms p = map modifyInstruction [0..(Vector.length p - 1)]
    where modifyInstruction i = Vector.modify (modifyInstruction' i) p
          modifyInstruction' i v = (newInstruction <$> read v i) >>= write v i
          newInstruction (Nop x) = Jmp x
          newInstruction (Jmp x) = Nop x
          newInstruction x = x

runB :: (HasLogFunc env) => RIO env ()
runB = do
    program <- readProgram "data/day08.txt"
    let success = filter (\(e, _) -> e == NormalExit)
                         (map (\p -> runProgram (Machine 0 0 p Set.empty))
                              (modifiedPrograms program))
    logInfo $ display $ tshow success
