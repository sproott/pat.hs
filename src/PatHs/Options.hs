{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
module PatHs.Options (commandP) where

import           Options.Applicative
import           PatHs.Options.Complete (keyCompleter)
import           PatHs.Types

commandP :: FilePath -> ParserInfo SomeCommand
commandP currentDirectory = info (commandParser currentDirectory <**> helper)
  ( fullDesc
  <> progDesc "Save often used directories like bookmarks"
  <> header "pat-hs - a terminal directory bookmark utility")

commandParser :: FilePath -> Parser SomeCommand
commandParser currentDirectory = subparser (
     command "save" (mkCommand (saveP currentDirectory) "Save bookmark")
  <> command "delete" (mkCommand deleteP "Delete bookmark")
  <> command "get" (mkCommand getP "Get bookmark")
  <> command "list" (mkCommand listP "List all bookmarks")
  )
  where
    mkCommand :: Parser (Command c) -> String -> ParserInfo SomeCommand
    mkCommand parser desc = info (SomeCommand <$> parser) $ progDesc desc

saveP :: FilePath -> Parser (Command 'Save)
saveP currentDirectory = CSave <$> keyP <*> pure (Value currentDirectory)

deleteP :: Parser (Command 'Delete)
deleteP = CDelete <$> keyP

getP :: Parser (Command 'Get)
getP = CGet <$> keyP

listP :: Parser (Command 'List)
listP = pure CList

keyP :: Parser Key
keyP = Key <$> argument str (metavar "KEY" <> completer keyCompleter)
