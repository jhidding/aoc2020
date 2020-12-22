module Day22 where

import RIO
import RIO.Seq ( (<|), (|>), ViewL((:<)), (><) )
import qualified RIO.Seq as Seq
import Parsing (Parser, readInput, string, integer, eol, sepEndBy1)

decks :: Parser (Seq Int, Seq Int)
decks = do
    string "Player 1:" >> eol
    player1 <- integer `sepEndBy1` eol
    eol
    string "Player 2:" >> eol
    player2 <- integer `sepEndBy1` eol
    return (Seq.fromList player1, Seq.fromList player2)

type Decks = (Seq Int, Seq Int)
type DeckViews = (Seq.ViewL Int, Seq.ViewL Int)

combatRound :: DeckViews -> Maybe Decks
combatRound (ca :< ra, cb :< rb)
    | ca > cb  = Just (ra |> ca |> cb, rb)
    | otherwise = Just (ra, rb |> cb |> ca)
combatRound _ = Nothing

score :: Seq Int -> Int
score d = sum $ zipWith (*) (reverse $ toList d) [1..]

play1 :: Decks -> Int
play1 (a, b) = case combatRound (Seq.viewl a, Seq.viewl b) of
    Nothing -> score (a >< b)
    Just n -> play1 n

type Deck = Seq Int
data Game = Game
    { gameHistory :: ([Deck], [Deck])
    , gameState   :: (Deck, Deck)
    }

data Outcome = Player1Wins Deck | Player2Wins Deck

recurRound :: Game -> Outcome
recurRound Game{..}
    | fst gameState `elem` fst gameHistory ||
      snd gameState `elem` snd gameHistory = Player1Wins (fst gameState)
    | otherwise = recur (Seq.viewl (fst gameState)) (Seq.viewl (snd gameState))
    where recur Seq.EmptyL (b :< bs) = Player2Wins (b <| bs)
          recur (a :< as) Seq.EmptyL = Player1Wins (a <| as)
          recur (a :< as) (b :< bs)
            | Seq.length as < a || Seq.length bs < b = nextRound $ if a > b
                    then (as |> a |> b, bs)
                    else (as, bs |> b |> a)
            | otherwise = case recurRound (Game ([],[]) (Seq.take a as, Seq.take b bs)) of
                Player1Wins _ -> nextRound (as |> a |> b, bs)
                Player2Wins _ -> nextRound (as, bs |> b |> a)
          nextRound = recurRound . Game ( fst gameState : fst gameHistory
                                        , snd gameState : snd gameHistory )

runA :: (HasLogFunc env) => RIO env ()
runA = do
    d <- readInput "data/day22.txt" decks
    logInfo $ display $ play1 d

runB :: (HasLogFunc env) => RIO env ()
runB = do
    d <- readInput "data/day22.txt" decks
    logInfo $ display $ case recurRound (Game ([],[]) d) of
        Player1Wins x -> score x
        Player2Wins x -> score x
