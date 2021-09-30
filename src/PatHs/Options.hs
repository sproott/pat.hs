{-# LANGUAGE DataKinds #-}

module PatHs.Options (commandP) where

import           Options.Applicative
import           PatHs.Options.Complete (keyCompleter)
import           PatHs.Types

commandP :: Value -> ParserInfo SomeCommand
commandP currentDirectory = info (commandParser currentDirectory <**> helper)
  ( fullDesc
  <> progDesc "Save often used directories like bookmarks"
  <> header "pat-hs - a terminal directory bookmark utility")

commandParser :: Value -> Parser SomeCommand
commandParser currentDirectory = subparser (
     command "save" (mkCommand (saveP currentDirectory) "Save bookmark")
  <> command "delete" (mkCommand deleteP "Delete bookmark")
  <> command "get" (mkCommand getP "Get bookmark")
  <> command "list" (mkCommand listP "List all bookmarks")
  )
  where
    mkCommand :: Parser (Command c) -> String -> ParserInfo SomeCommand
    mkCommand parser desc = info (SomeCommand <$> parser) $ progDesc desc

saveP :: Value -> Parser (Command 'Save)
saveP currentDirectory = CSave <$> keyP False <*> pure currentDirectory

deleteP :: Parser (Command 'Delete)
deleteP = CDelete <$> keyP True

getP :: Parser (Command 'Get)
getP = CGet <$> keyP True

listP :: Parser (Command 'List)
listP = pure CList

keyP :: Bool -> Parser Key
keyP addCompleter = Key <$> argument str (metavar "KEY" <> if addCompleter then completer keyCompleter else mempty)
