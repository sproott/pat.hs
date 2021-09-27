{-# LANGUAGE OverloadedStrings #-}

module PatHs.Config (Config(..), configParser) where

import           Control.Applicative  (many, (<|>))
import           Control.Arrow        ((***))
import           Data.Text            (Text)
import           Data.Void            (Void)
import           PatHs.Types
import           Text.Megaparsec      (Parsec)
import           Text.Megaparsec.Char (alphaNumChar, char, eol, letterChar,
                                       printChar)

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
  eol
  pure (key, value)

file :: Parser [(String, String)]
file = many line

configParser :: Parser Config
configParser = fmap (Key *** Value) <$> file
