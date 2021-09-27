module PatHs.Config.Common where
import           Control.Applicative  (empty, many, (<|>))
import           Control.Arrow        (left)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, runParser)
import           Text.Megaparsec.Char (alphaNumChar, char, eol, letterChar,
                                       printChar)

type Parser = Parsec Void Text

ident :: Parser String
ident = do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  return (c:cs)

line :: Parser (String, String)
line = do
  key <- ident
  _ <- char '='
  value <- many printChar
  eol
  pure (key, value)

file :: Parser [(String, String)]
file = many line

parse :: e -> Parser a -> String -> Either e a
parse error parser contents = left (const error) $ runParser parser empty $ Text.pack contents
