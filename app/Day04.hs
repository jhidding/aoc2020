module Day04 where

import RIO hiding (some, many, try)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Text.Megaparsec
    ( chunk,
      getOffset,
      parse,
      satisfy,
      single,
      errorBundlePretty,
      count,
      sepEndBy1,
      Parsec,
      MonadParsec(try, takeWhile1P, lookAhead, takeP),
      ParseErrorBundle )
import Text.Megaparsec.Char (string, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isSpace)
import qualified RIO.Text as Text
-- import Debug.Trace (trace)

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

data Unit = Cm | Inch deriving (Eq, Show)
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth deriving (Eq, Show)
data FieldData = Invalid | Unknown | Year Int | Length Int Unit | Color Int 
    | EyeColorCode EyeColor | ID Text
    deriving (Show, Eq)

data FieldID
    = BirthYear
    | IssueYear
    | ExpirationYear
    | Height
    | HairColor
    | EyeColor
    | PasspordID
    | CountryID
    deriving (Show, Eq, Ord)

data FieldDescr = FieldDescr
    { fieldID :: FieldID
    , fieldParser :: Parser FieldData }

invalidValue :: Parser FieldData
invalidValue = do
    takeWhile1P (Just "value") (not . isSpace)
    return Invalid

year :: Int -> Int -> Parser FieldData
year min max = do
    y <- L.decimal
    return $ if y >= min && y <= max then Year y else Invalid

height :: Parser FieldData
height = do
    num <- L.decimal
    unit <- (try (chunk "cm") >> return Cm) <|> (chunk "in" >> return Inch)
    validated num unit
    where validate len Cm = len >= 150 && len <= 193
          validate len Inch = len >= 59 && len <= 76
          validated len unit = if validate len unit then return (Length len unit) else fail "invalid length"

exactlyN :: Int -> Parser a -> Parser a
exactlyN n p = do
    i <- getOffset
    x <- p
    j <- getOffset
    if j - i == n then return x else fail ("expected lexeme of " <> show n <> " characters")

color :: Parser FieldData
color = single '#' >> Color <$> exactlyN 6 L.hexadecimal

eyeColor :: Parser FieldData
eyeColor = EyeColorCode <$> ((string "amb" >> pure Amb)
                         <|> (string "blu" >> pure Blu)
                         <|> (string "brn" >> pure Brn)
                         <|> (string "gry" >> pure Gry)
                         <|> (string "grn" >> pure Grn)
                         <|> (string "hzl" >> pure Hzl)
                         <|> (string "oth" >> pure Oth))

passportId :: Parser FieldData
passportId = ID . Text.pack <$> count 9 digitChar

countryId :: Parser FieldData
countryId = invalidValue >> pure Unknown

validKeys :: Map Text FieldDescr
validKeys = Map.fromList
    [ ("byr", FieldDescr BirthYear      $ year 1920 2002)
    , ("iyr", FieldDescr IssueYear      $ year 2010 2020)
    , ("eyr", FieldDescr ExpirationYear $ year 2020 2030)
    , ("hgt", FieldDescr Height         height)
    , ("hcl", FieldDescr HairColor      color)
    , ("ecl", FieldDescr EyeColor       eyeColor)
    , ("pid", FieldDescr PasspordID     passportId)
    , ("cid", FieldDescr CountryID      countryId)
    ]

entireValue :: Parser a -> Parser a
entireValue p = do
    x <- p
    lookAhead (satisfy isSpace)
    return x

keyValuePair :: Parser (FieldID, FieldData)
keyValuePair = do
    FieldDescr{..} <- try fieldDescr
    _ <- single ':'
    d <- try (entireValue fieldParser) <|> invalidValue
    return (fieldID, d)
    where fieldDescr :: Parser FieldDescr
          fieldDescr = do
            code <- takeP (Just "field code") 3
            let k = validKeys Map.!? code
            maybe (fail "expected valid field code") return k
                  
type Record = [(FieldID, FieldData)]

oneRecord :: Parser Record
oneRecord = keyValuePair `sepEndBy1` (try (chunk " ") <|> try (chunk "\n"))

entries :: Parser [Record]
entries = oneRecord `sepEndBy1` chunk "\n"

readData :: (HasLogFunc env) => RIO env [Record]
readData = do
    text <- readFileUtf8 "data/day04.txt"
    either (\e -> do { logError $ display e; return [] } )
           return $ parse entries "data/day04.txt" text

validateRecord1 :: Record -> Bool
validateRecord1 r
    | Set.null missing = True
    | missing == Set.singleton CountryID = True
    | otherwise = False
    where missing = allIds Set.\\ present
          present = Set.fromList (map fst r)
          allIds = Set.fromList (map fieldID $ Map.elems validKeys)

validateRecord2 :: Record -> Bool
validateRecord2 r = validateRecord1 r && all ((/= Invalid) . snd) r

runA :: (HasLogFunc env) => RIO env ()
runA = do
    d <- readData
    logInfo $ display $ length $ filter validateRecord1 d

runB :: (HasLogFunc env) => RIO env ()
runB = do
    d <- readData
    logInfo $ display $ length $ filter validateRecord2 d
