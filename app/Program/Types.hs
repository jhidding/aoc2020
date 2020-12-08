module Program.Types where

import RIO

data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int
    deriving (Show)

type Program = Vector Instruction

data Machine = Machine
    { ax :: Int
    , cx :: Int
    , program :: Program
    , passes :: Set Int
    } deriving (Show)

data ProgramExitCode
    = NormalExit
    | NonTerminating
    deriving (Show, Eq)