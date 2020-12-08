module Main where

import RIO
import qualified RIO.Map as Map
import Options.Applicative

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08

solutions :: Map Int (RIO SimpleApp (), RIO SimpleApp ())
solutions = Map.fromList
    [ (1, (Day01.runA, Day01.runB))
    , (2, (Day02.runA, Day02.runB))
    , (3, (Day03.runA, Day03.runB))
    , (4, (Day04.runA, Day04.runB))
    , (5, (Day05.runA, Day05.runB))
    , (6, (Day06.runA, Day06.runB))
    , (7, (Day07.runA, Day07.runB))
    , (8, (Day08.runA, Day08.runB))
    ]

data CommandArgs = CommandArgs
    { dayArg :: Maybe Int
    , runAll :: Bool }

commandArgs :: Parser CommandArgs
commandArgs = CommandArgs <$> optional
                            (  option auto
                            $  long "day" <> short 'd' 
                            <> help "run solution for this day, defaults to latest"
                            <> metavar "INT" )
                          <*> switch ( long "all" <> short 'a' <> help "Run everything" )        

args :: ParserInfo CommandArgs
args = info (commandArgs <**> helper)
            ( fullDesc
            <> progDesc "Run solutions for Advent of Code 2020"
            <> header "x2020 - a puzzle solver")

main :: IO ()
main = runSimpleApp $ do
    args <- liftIO $ execParser args
    if runAll args then
        mapM_ (uncurry runSolution) (Map.toList solutions)
    else fromMaybe (logInfo "No solution for that day") $ do
        d <- day args
        s <- solution d
        Just $ runSolution d s
    where day args = (fst <$> Map.lookupMax solutions) <|> dayArg args
          solution day = (snd <$> Map.lookupMax solutions) <|> (solutions Map.!? day)
          runSolution n (a, b) = logInfo (display $ "Day " <> tshow n <> " ========")
                               >> a >> b >> logInfo ""
