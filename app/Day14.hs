{-# LANGUAGE TupleSections #-}
module Day14 where

import RIO hiding (try)
import RIO.State
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Map as Map

import Data.Bits
import Data.Char (digitToInt)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char ( char, eol )
import Text.Megaparsec.Char.Lexer ( decimal )

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

data Machine1Data = Machine1Data
    { machineMemory :: Map Int Int
    , machineMask :: Int -> Int }

newtype Machine1 a = Machine1 { runMachine :: State Machine1Data a }
    deriving (Functor, Applicative, Monad, MonadState Machine1Data)

class Monad m => MonadMachine m where
    setMask :: Text -> m ()
    writeValue :: Int -> Int -> m ()
    sumValues :: m Int

instance MonadMachine Machine1 where
    setMask t = modify (\m@Machine1Data{..} -> m {
        machineMask = makeMask1 t })
    writeValue a b = modify (\m@Machine1Data{..} -> m { 
        machineMemory = Map.insert a (machineMask b) machineMemory })
    sumValues = sum . Map.elems <$> gets machineMemory

data Machine2Data = Machine2Data
    { machine2Memory :: Map Int Int
    , machine2Mask :: Int -> [Int] }

newtype Machine2 a = Machine2 { runMachine2 :: State Machine2Data a }
    deriving (Functor, Applicative, Monad, MonadState Machine2Data)

instance MonadMachine Machine2 where
    setMask t = modify (\m@Machine2Data{..} -> m {
        machine2Mask = makeMask2 t })
    writeValue a b = modify (\m@Machine2Data{..} -> m {
        machine2Memory = Map.union (Map.fromList $ map (,b) (machine2Mask a)) machine2Memory })
    sumValues = sum . Map.elems <$> gets machine2Memory

toDec :: Text -> Int
toDec = Text.foldl' (\acc x -> acc * 2 + digitToInt x) 0

makeMask1 :: Text -> Int -> Int
makeMask1 m = applyOnes . applyZeroes
    where applyOnes = (.|. oneMask)
          applyZeroes = (.&. zeroMask)
          oneMask = toDec $ Text.map (\c -> if c == '1' then '1' else '0') m
          zeroMask = toDec $ Text.map (\c -> if c == '0' then '0' else '1') m

makeMask2 :: Text -> Int -> [Int]
makeMask2 m n = expand bits
    where bits = zip [35,34..0] (Text.unpack  m)
          expand ((bit, v) : rest) | v == '0' = map ((2^bit .&. n) +) $ expand rest
                                   | v == '1' = map (2^bit +) $ expand rest
                                   | v == 'X' = concatMap (\v -> [2^bit + v, v]) $ expand rest
          expand []                           = [0]

maskInstruction :: MonadMachine m => Parser (m ())
maskInstruction = do
    _ <- chunk "mask = "
    mask <- takeWhileP (Just "bitmask") (`elem` ['0', '1', 'X'])
    return (setMask mask)

writeInstruction :: MonadMachine m => Parser (m ())
writeInstruction = do
    _ <- chunk "mem["
    a <- decimal
    _ <- chunk "] = "
    writeValue a <$> decimal

instruction :: MonadMachine m => Parser (m ())
instruction = try maskInstruction <|> try writeInstruction

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env, MonadMachine x) => m [x ()]
readInput = do
    x <- parse (instruction `sepEndBy1` eol) "data/day14.txt" <$> readFileUtf8 "data/day14.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return x

runA :: (HasLogFunc env) => RIO env ()
runA = do
    instr <- readInput
    let result = evalState (runMachine $ sequence_ instr >> sumValues)
                           (Machine1Data Map.empty id)
    logInfo $ display result

runB :: (HasLogFunc env) => RIO env ()
runB = do
    instr <- readInput
    let result = evalState (runMachine2 $ sequence_ instr >> sumValues)
                           (Machine2Data Map.empty (:[]))
    logInfo $ display result
