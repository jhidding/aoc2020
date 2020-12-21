module Day21 where

import RIO hiding (try)

import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified RIO.Set as Set

import RIO.List.Partial (foldl1')

import Text.Megaparsec
    ( parse, sepBy1, sepEndBy1, Parsec )
import Text.Megaparsec.Char ( char, hspace, eol, string, letterChar )
import qualified Text.Megaparsec.Char.Lexer as L

import Day16 (findMatch)

type Parser = Parsec Void Text

data FoodProduct = FoodProduct
    { _ingredients :: Set Text
    , _allergens   :: Set Text
    } deriving (Show)

ingredients :: Lens' FoodProduct (Set Text)
ingredients = lens _ingredients (\i x -> i { _ingredients = x })

allergens :: Lens' FoodProduct (Set Text)
allergens = lens _allergens (\i x -> i { _allergens = x })

readInput :: (MonadReader env m, MonadIO m, HasLogFunc env) => FilePath -> Parser a -> m a
readInput file p = do
    x <- parse p file <$> readFileUtf8 file
    either (\e -> do { logError $ display e; exitFailure })
           return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

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
                   $ filter (\f -> a `Set.member` (f^.allergens)) foods

runA :: (HasLogFunc env) => RIO env ()
runA = do
    x <- readInput  "data/day21.txt" (foodProduct `sepEndBy1` eol)
    let allIngredients = mconcat $ map (^. ingredients) x
        allAllergens   = mconcat $ map (^. allergens) x
        c = Set.unions $ map (candidates x) (Set.toList allAllergens)
        safe = allIngredients Set.\\ c
        count = sum $ map (\f -> Set.size (Set.intersection (f^.ingredients) safe)) x
    logInfo $ display $ tshow count

runB :: (HasLogFunc env) => RIO env ()
runB = do
    x <- readInput  "data/day21.txt" (foodProduct `sepEndBy1` eol)
    let allAllergens   = mconcat $ map (^. allergens) x
        unsafeIngredients = Set.unions $ map (candidates x) (Set.toList allAllergens)
        redacted = map (ingredients %~ Set.intersection unsafeIngredients) x
        matchAtoI a i  = i `Set.member` candidates redacted a
        matches = findMatch matchAtoI (Set.toList allAllergens) (Set.toList unsafeIngredients)
        cdil = Text.intercalate "," $ map snd $ List.sortOn fst matches
    logInfo $ display cdil
