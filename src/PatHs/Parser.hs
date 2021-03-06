module PatHs.Parser where

import Control.Arrow (left)
import qualified Data.Text as T
import PatHs.Prelude hiding (many, optional)
import System.FilePath (pathSeparator)
import Text.Megaparsec (MonadParsec (eof), Parsec, many, optional, runParser)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    eol,
    letterChar,
    printChar,
  )

type Parser = Parsec Void Text

ident :: Parser Text
ident = do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '-')
  pure $ T.pack (c : cs)

line :: Parser (Text, Text)
line = do
  key <- ident
  _ <- char '='
  value <- many printChar
  _ <- eol
  pure (key, T.pack value)

file :: Parser [(Text, Text)]
file = many line

parseGoPath :: Parser (Text, Maybe Text)
parseGoPath = do
  key <- ident
  goPath <- optional $ do
    _ <- char pathSeparator
    many printChar
  eof
  pure (key, T.pack <$> goPath)

parse :: e -> Parser a -> Text -> Either e a
parse err parser contents = left (const err) $ runParser parser empty contents
