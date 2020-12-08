module Day08 where

import RIO
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector
import qualified RIO.Set as Set
import RIO.State ( MonadState(get, put), evalState, State, modify, gets )
import Data.Vector.Generic.Mutable (write, read)

import Text.Megaparsec
    ( chunk,
      parse,
      errorBundlePretty,
      sepEndBy1,
      Parsec,
      ParseErrorBundle )
import Text.Megaparsec.Char ( eol, hspace )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

integer :: Parser Int
integer = lexeme L.decimal

number :: Parser Int
number = L.signed hspace integer

data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int
    deriving (Show)

nop :: Parser Instruction
nop = Nop <$> (lexeme (chunk "nop") >> number)

acc :: Parser Instruction
acc = Acc <$> (lexeme (chunk "acc") >> number)

jmp :: Parser Instruction
jmp = Jmp <$> (lexeme (chunk "jmp") >> number)

instruction :: Parser Instruction
instruction = nop <|> acc <|> jmp

programP :: Parser (Vector Instruction)
programP = Vector.fromList <$> instruction `sepEndBy1` eol

readProgram :: (MonadIO m, HasLogFunc env, MonadReader env m) => m (Vector Instruction)
readProgram = do
    program <- parse programP "data/day08.txt" <$> readFileUtf8 "data/day08.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return program

data Machine = Machine
    { accumulator :: Int
    , instructionPointer :: Int
    , program :: Vector Instruction
    , passes :: Set Int
    } deriving (Show)

readInstruction :: Machine -> Maybe Instruction
readInstruction Machine{..} = program Vector.!? instructionPointer

flagPass :: MonadState Machine m => m ()
flagPass = modify (\m -> m { passes = Set.insert (instructionPointer m) (passes m) })

runInstruction :: MonadState Machine m => m ()
runInstruction = do
    flagPass
    m@Machine{..} <- get
    case program Vector.!? instructionPointer of
        Just (Nop _) -> put $ m { instructionPointer = instructionPointer + 1 }
        Just (Acc d) -> put $ m { accumulator = accumulator + d
                                , instructionPointer = instructionPointer + 1 }
        Just (Jmp d) -> put $ m { instructionPointer = instructionPointer + d }
        Nothing      -> return ()

nonTerminating :: MonadState Machine m => m Bool
nonTerminating = do
    Machine{..} <- get
    return $ Set.member instructionPointer passes

endOfProgram :: MonadState Machine m => m Bool
endOfProgram = do
    Machine{..} <- get
    return $ instructionPointer >= Vector.length program

data ProgramExitCode = NormalExit | NonTerminating deriving (Show, Eq)

exit :: MonadState Machine m => ProgramExitCode -> m (ProgramExitCode, Int)
exit c = do
    acc <- gets accumulator
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

type Program = Vector Instruction

modifiedPrograms :: Program -> [Program]
modifiedPrograms p = map modifyInstruction [0..(Vector.length p - 1)]
    where modifyInstruction i = Vector.modify (modifyInstruction' i) p
          modifyInstruction' i v = (newInstruction <$> read v i) >>= write v i
          newInstruction (Nop x) = Jmp x
          newInstruction (Jmp x) = Nop x
          newInstruction x = x

runA :: (HasLogFunc env) => RIO env ()
runA = do
    program <- readProgram
    logInfo $ display $ tshow $ runProgram (Machine 0 0 program Set.empty)

runB :: (HasLogFunc env) => RIO env ()
runB = do
    program <- readProgram
    let success = filter (\(e, _) -> e == NormalExit) (map (\p -> runProgram (Machine 0 0 p Set.empty)) (modifiedPrograms program))
    logInfo $ display $ tshow success
