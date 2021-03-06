module PatHs.Options (commandP) where

import Data.Either.Extra (eitherToMaybe)
import Options.Applicative
import PatHs.Options.Complete
import PatHs.Prelude
import PatHs.Types

commandP :: ParserInfo SomeCommand
commandP =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Save often used directories like bookmarks"
        <> header "pat.hs - a terminal directory bookmark utility"
    )

commandParser :: Parser SomeCommand
commandParser =
  subparser
    ( command "save" (mkCommand saveP "Save bookmark")
        <> command "delete" (mkCommand deleteP "Delete bookmark")
        <> command "rename" (mkCommand renameP "Rename bookmark")
        <> command "get" (mkCommand getP "Get bookmark")
        <> command "go" (mkCommand goP "Go to a directory related to bookmark")
        <> command "list" (mkCommand listP "List all bookmarks")
    )
  where
    mkCommand :: Parser (Command c) -> String -> ParserInfo SomeCommand
    mkCommand parser desc = info (SomeCommand <$> parser) $ progDesc desc

saveP :: Parser (Command Save)
saveP = CSave <$> keyP False

deleteP :: Parser (Command Delete)
deleteP = CDelete <$> keyP True

renameP :: Parser (Command Rename)
renameP = CRename <$> keyP True <*> (Key <$> strArgument (metavar "NEW_KEY"))

getP :: Parser (Command Get)
getP = CGet <$> keyP True

goP :: Parser (Command Go)
goP = mkCGo <$> strArgument (metavar "GO_PATH" <> completer (mkCompleter' goPathCompleterIO))

mkCGo :: Text -> Command Go
mkCGo s = CGo $ eitherToMaybe $ mkGoPath s

listP :: Parser (Command List)
listP = pure CList

keyP :: Bool -> Parser Key
keyP addCompleter = Key <$> strArgument (metavar "KEY" <> if addCompleter then completer (mkCompleter' keyCompleter) else mempty)
