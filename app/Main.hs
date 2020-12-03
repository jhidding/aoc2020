module Main where

import RIO
import qualified RIO.Map as Map
import Options.Applicative

import qualified Day01
import qualified Day02
import qualified Day03

solutions :: HasLogFunc env => Map Int (RIO env (), RIO env ())
solutions = Map.fromList
    [ (1, (Day01.runA, Day01.runB))
    , (2, (Day02.runA, Day02.runB))
    , (3, (Day03.runA, Day03.runB)) ]

newtype CommandArgs = CommandArgs
    { dayArg :: Maybe Int }

commandArgs :: Parser CommandArgs
commandArgs = CommandArgs <$> optional
                (  option auto
                $  long "day" <> short 'd' 
                <> help "run solution for this day, defaults to latest"
                <> metavar "INT" )

args :: ParserInfo CommandArgs
args = info (commandArgs <**> helper)
            ( fullDesc
            <> progDesc "Run solutions for Advent of Code 2020"
            <> header "x2020 - a puzzle solver")

main :: IO ()
main = runSimpleApp $ do
    args <- liftIO $ execParser args
    let solution = maybe (fmap snd (Map.lookupMax solutions))
                         (solutions Map.!?)
                         (dayArg args)
    case solution of
        Nothing -> logInfo "NYI"
        Just (a, b) -> a >> b