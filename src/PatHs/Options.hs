{-# LANGUAGE DataKinds #-}

module PatHs.Options (commandP) where

import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Options.Applicative
import PatHs.Options.Complete
import PatHs.Types

commandP :: HomeDir -> Value -> ParserInfo SomeCommand
commandP homeDir currentDirectory =
  info
    (commandParser homeDir currentDirectory <**> helper)
    ( fullDesc
        <> progDesc "Save often used directories like bookmarks"
        <> header "pat.hs - a terminal directory bookmark utility"
    )

commandParser :: HomeDir -> Value -> Parser SomeCommand
commandParser homeDir currentDirectory =
  subparser
    ( command "save" (mkCommand (saveP currentDirectory) "Save bookmark")
        <> command "delete" (mkCommand deleteP "Delete bookmark")
        <> command "rename" (mkCommand renameP "Rename bookmark")
        <> command "get" (mkCommand getP "Get bookmark")
        <> command "go" (mkCommand (goP homeDir) "Go to a directory related to bookmark")
        <> command "list" (mkCommand listP "List all bookmarks")
    )
  where
    mkCommand :: Parser (Command c) -> String -> ParserInfo SomeCommand
    mkCommand parser desc = info (SomeCommand <$> parser) $ progDesc desc

saveP :: Value -> Parser (Command 'Save)
saveP currentDirectory = CSave <$> keyP False <*> pure currentDirectory

deleteP :: Parser (Command 'Delete)
deleteP = CDelete <$> keyP True

renameP :: Parser (Command 'Rename)
renameP = CRename <$> keyP True <*> (Key <$> argument str (metavar "NEW_KEY"))

getP :: Parser (Command 'Get)
getP = CGet <$> keyP True

goP :: HomeDir -> Parser (Command 'Go)
goP homeDir = mkCGo homeDir <$> argument str (metavar "GO_PATH" <> completer (mkCompleter' goPathCompleter))

mkCGo :: HomeDir -> Text -> Command 'Go
mkCGo homeDir str = CGo homeDir $ eitherToMaybe $ mkGoPath str

listP :: Parser (Command 'List)
listP = pure CList

keyP :: Bool -> Parser Key
keyP addCompleter = Key <$> argument str (metavar "KEY" <> if addCompleter then completer (mkCompleter' keyCompleter) else mempty)
