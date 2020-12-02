module Day02 where

import RIO hiding (many)
import RIO.Char ( isLetter )

import qualified RIO.Text as Text
import Text.Megaparsec
    ( parse,
      errorBundlePretty,
      sepEndBy,
      Parsec,
      MonadParsec(takeWhile1P),
      ParseErrorBundle )
import Text.Megaparsec.Char ( hspace, letterChar, newline )
import qualified Text.Megaparsec.Char.Lexer as L

data PasswordEntry = PasswordEntry
    { minAmount :: Int
    , maxAmount :: Int
    , charSpec :: Char
    , password :: Text
    } deriving (Show)

type Parser = Parsec Void Text

instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

passwordEntry :: Parser PasswordEntry
passwordEntry = do
    minAmount <- integer
    _ <- symbol "-"
    maxAmount <- integer
    charSpec <- letter
    _ <- symbol ":"
    PasswordEntry minAmount maxAmount charSpec <$> word
    where symbol = L.symbol hspace
          integer = L.lexeme hspace L.decimal
          letter = L.lexeme hspace letterChar
          word = L.lexeme hspace (takeWhile1P Nothing isLetter)

passwordEntries :: Parser [PasswordEntry]
passwordEntries = passwordEntry `sepEndBy` newline

readData :: (HasLogFunc env, MonadReader env m, MonadIO m) => m [PasswordEntry]
readData = do
    text <- readFileUtf8 "data/day02.txt"
    case parse passwordEntries "data/day02.txt" text of
        Right passwords -> return passwords
        Left err -> do
            logError $ display err
            return []

validEntry1 :: PasswordEntry -> Bool
validEntry1 PasswordEntry{..} = count >= minAmount && count <= maxAmount
    where count = Text.length (Text.filter (== charSpec) password)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

validEntry2 :: PasswordEntry -> Bool
validEntry2 PasswordEntry{..} = (ref1 == charSpec) `xor` (ref2 == charSpec)
    where ref1 = Text.index password (minAmount - 1)
          ref2 = Text.index password (maxAmount - 1)

runA :: HasLogFunc env => RIO env ()
runA = do
    entries <- readData
    logInfo $ display $ length $ filter validEntry1 entries

runB :: HasLogFunc env => RIO env ()
runB = do
    entries <- readData
    logInfo $ display $ length $ filter validEntry2 entries
