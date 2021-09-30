module PatHs.Config.Common where
import           Control.Applicative  (empty, (<|>))
import           Control.Arrow        (left)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, many, runParser)
import           Text.Megaparsec.Char (alphaNumChar, char, eol, letterChar,
                                       printChar)

type Parser = Parsec Void Text

ident :: Parser Text
ident = do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_')
  pure $ Text.pack (c:cs)

line :: Parser (Text, Text)
line = do
  key <- ident
  _ <- char '='
  value <- many printChar
  eol
  pure (key, Text.pack value)

file :: Parser [(Text, Text)]
file = many line

parse :: e -> Parser a -> Text -> Either e a
parse error parser contents = left (const error) $ runParser parser empty contents
