{-# LANGUAGE OverloadedStrings #-}

module PatHs.Config (Config(..), configParser) where

import           Control.Arrow              ((***))
import           Data.Char                  (isSpace)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Void                  (Void)
import           PatHs.Types
import           Text.Megaparsec            (Parsec, empty, many, optional,
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             printChar, space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Config = [(Key, Value)]

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
  pure (key, value)

file :: Parser [(String, String)]
file = many line

configParser :: Parser Config
configParser = fmap (Key *** Value) <$> file
