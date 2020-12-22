module Day21 where

import RIO hiding (try)

import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Set as Set

import RIO.List.Partial (foldl1')

import Text.Megaparsec.Char ( letterChar )
import Parsing
    ( sepBy1, sepEndBy1, char, eol, string, Parser, readInput, lexeme )
import Day16 (findMatch)

data FoodProduct = FoodProduct
    { _ingredients :: Set Text
    , _allergens   :: Set Text
    } deriving (Show)

ingredients :: Lens' FoodProduct (Set Text)
ingredients = lens _ingredients (\i x -> i { _ingredients = x })

allergens :: Lens' FoodProduct (Set Text)
allergens = lens _allergens (\i x -> i { _allergens = x })

word :: Parser Text
word = lexeme (Text.pack <$> some letterChar)

foodProduct :: Parser FoodProduct
foodProduct = do
    i <- Set.fromList <$> some word
    _ <- lexeme $ string "(contains"
    a <- Set.fromList <$> word `sepBy1` lexeme (char ',')
    _ <- lexeme $ string ")"
    return $ FoodProduct i a

candidates :: [FoodProduct] -> Text -> Set Text
candidates foods a = foldl1' Set.intersection
                   $ map (^.ingredients)
                   $ filter (^. allergens . to (Set.member a)) foods

runA :: (HasLogFunc env) => RIO env ()
runA = do
    foods <- readInput "data/day21.txt" (foodProduct `sepEndBy1` eol)
    let allAllergens = mconcat $ map (^. allergens) foods
        unsafe = Set.unions $ map (candidates foods) (Set.toList allAllergens)
        count = sum $ map (^. ingredients . to (Set.size . (Set.\\ unsafe))) foods
    logInfo $ display $ tshow count

runB :: (HasLogFunc env) => RIO env ()
runB = do
    x <- readInput  "data/day21.txt" (foodProduct `sepEndBy1` eol)
    let allAllergens   = mconcat $ map (^. allergens) x
        unsafe = Set.unions $ map (candidates x) (Set.toList allAllergens)
        redacted = map (ingredients %~ Set.intersection unsafe) x
        matchAtoI a i  = i `Set.member` candidates redacted a
        matches = findMatch matchAtoI (Set.toList allAllergens) (Set.toList unsafe)
        cdil = Text.intercalate "," $ map snd $ List.sortOn fst matches
    logInfo $ display cdil
