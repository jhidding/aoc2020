module Day07 where

import RIO hiding (some, many, try)
import qualified RIO.Text as Text
import qualified RIO.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import Data.Graph


type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

data Rule = Rule
    { outerBag :: Text
    , innerBags :: [(Text, Int)]
    } deriving (Show)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

word :: Parser Text
word = lexeme $ do
    x <- some letterChar
    if x == "bag" || x == "bags" then
        fail "unexpected: bag(s)"
    else
        return $ Text.pack x

bag :: Parser Text
bag = do
    descr <- some (try word)
    _ <- lexeme (string "bag" >> optional (string "s"))
    return $ Text.intercalate " " descr

nBags :: Parser (Text, Int)
nBags = do
    n <- lexeme L.decimal
    b <- bag
    return (b, n)

rule :: Parser Rule
rule = do
    outerBag <- bag
    _ <- lexeme $ string "contain"
    innerBags <- try (nBags `sepBy1` lexeme (symbol ",")) <|> (string "no other bags" >> return [])
    _ <- lexeme $ symbol "."
    return Rule{..}
    where symbol = L.symbol hspace

readData :: (HasLogFunc env, MonadReader env m, MonadIO m) => m [Rule]
readData = do
    rules <- parse (rule `sepEndBy1` eol) "data/day07.txt" <$> readFileUtf8 "data/day07.txt"
    either (\e -> do { logError $ display e; return [] })
           return rules

contains :: Map Text [(Text, Int)] -> Text -> Text -> Bool
contains ruleMap a b = case ruleMap Map.!? a of
        Just rs -> any (\(c,_) -> c == b || contains ruleMap c b) rs
        Nothing -> False

toRuleMap :: [Rule] -> Map Text [(Text, Int)]
toRuleMap rules = Map.fromList (map (\Rule{..} -> (outerBag, innerBags)) rules)

bagSum :: Map Text [(Text, Int)] -> Text -> Int
bagSum ruleMap a = case ruleMap Map.!? a of
    Nothing -> 0
    Just rs -> 1 + sum (map (\(b, n) -> n * bagSum ruleMap b) rs)

runA :: (HasLogFunc env) => RIO env ()
runA = do
    ruleMap <- toRuleMap <$> readData
    let bagList = Map.keys ruleMap
    logInfo $ display $ length $ filter (\b -> contains ruleMap b "shiny gold") bagList

runB :: (HasLogFunc env) => RIO env ()
runB = do
    ruleMap <- toRuleMap <$> readData
    logInfo $ display $ bagSum ruleMap "shiny gold" - 1
