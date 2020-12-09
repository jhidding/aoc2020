module Program.Run (runProgram) where

import RIO
import qualified RIO.Vector as Vector
import qualified RIO.Set as Set
import RIO.State ( MonadState(get, put), evalState, State, modify, gets )

import Program.Types
    ( ProgramExitCode(..), Machine(..), Instruction(Jmp, Nop, Acc) )

flagPass :: MonadState Machine m => m ()
flagPass = modify (\m -> m { passes = Set.insert (cx m) (passes m) })

runInstruction :: MonadState Machine m => m ()
runInstruction = do
    flagPass
    m@Machine{..} <- get
    case program Vector.!? cx of
        Just (Nop _) -> put $ m { cx = cx + 1 }
        Just (Acc d) -> put $ m { ax = ax + d
                                , cx = cx + 1 }
        Just (Jmp d) -> put $ m { cx = cx + d }
        Nothing      -> return ()

nonTerminating :: MonadState Machine m => m Bool
nonTerminating = do
    Machine{..} <- get
    return $ Set.member cx passes

endOfProgram :: MonadState Machine m => m Bool
endOfProgram = do
    Machine{..} <- get
    return $ cx >= Vector.length program

exit :: MonadState Machine m => ProgramExitCode -> m (ProgramExitCode, Int)
exit c = do
    acc <- gets ax
    return (c, acc)

selectM :: Monad m => m a -> [(m Bool, m a)] -> m a
selectM d [] = d
selectM d ((cond, x) : rest) = do
    cond' <- cond
    if cond' then x else selectM d rest

runProgram :: Machine -> (ProgramExitCode, Int)
runProgram = evalState go
    where go :: State Machine (ProgramExitCode, Int)
          go = runInstruction >>
               selectM go
                  [ (nonTerminating, exit NonTerminating)
                  , (endOfProgram, exit NormalExit) ]