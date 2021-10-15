module PatHs.Parser where

import Control.Arrow (left)
import qualified Data.Text as Text
import System.FilePath (pathSeparator)
import Text.Megaparsec (Parsec, many, optional, runParser)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    eol,
    letterChar,
    printChar,
  )
import Prelude hiding (many, optional)

type Parser = Parsec Void Text

ident :: Parser Text
ident = do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '-')
  pure $ Text.pack (c : cs)

line :: Parser (Text, Text)
line = do
  key <- ident
  _ <- char '='
  value <- many printChar
  eol
  pure (key, Text.pack value)

file :: Parser [(Text, Text)]
file = many line

splitGoPath :: Parser (Maybe Text, Maybe Text)
splitGoPath = do
  key <- optional ident
  goPath <- optional $ do
    _ <- char pathSeparator
    many printChar
  pure (key, Text.pack <$> goPath)

parse :: e -> Parser a -> Text -> Either e a
parse error parser contents = left (const error) $ runParser parser empty contents
