module Program.Parser where

import RIO
import qualified RIO.Text as Text
import qualified RIO.Vector as Vector

import Text.Megaparsec
    ( chunk,
      parse,
      errorBundlePretty,
      sepEndBy1,
      Parsec,
      ParseErrorBundle )
import Text.Megaparsec.Char ( eol, hspace )
import qualified Text.Megaparsec.Char.Lexer as L

import Program.Types

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

integer :: Parser Int
integer = lexeme L.decimal

number :: Parser Int
number = L.signed hspace integer

nop :: Parser Instruction
nop = Nop <$> (lexeme (chunk "nop") >> number)

acc :: Parser Instruction
acc = Acc <$> (lexeme (chunk "acc") >> number)

jmp :: Parser Instruction
jmp = Jmp <$> (lexeme (chunk "jmp") >> number)

instruction :: Parser Instruction
instruction = nop <|> acc <|> jmp

programP :: Parser Program
programP = Vector.fromList <$> instruction `sepEndBy1` eol

readProgram :: (MonadIO m, HasLogFunc env, MonadReader env m) => FilePath -> m Program
readProgram path = do
    program <- parse programP path <$> readFileUtf8 path
    either (\e -> do { logError $ display e; exitFailure })
           return program