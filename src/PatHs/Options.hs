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
        <> command "remove" (mkCommand removeP "Remove one or more bookmarks")
        <> command "rename" (mkCommand renameP "Rename bookmark")
        <> command "get" (mkCommand getP "Get bookmark")
        <> command "go" (mkCommand goP "Go to a directory related to bookmark")
        <> command "list" (mkCommand listP "List all bookmarks")
    )
  where
    mkCommand :: Parser (Command c) -> String -> ParserInfo SomeCommand
    mkCommand parser desc = info (SomeCommand <$> parser) $ progDesc desc

saveP :: Parser (Command Save)
saveP = CSave <$> keyP False <*> switch (
  long "force" 
  <> short 'f' 
  <> help "Overwrite existing bookmark" )

removeP :: Parser (Command Remove)
removeP = CRemove <$> some (keyP' True Plural)

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

data Plurality = Plural | Singular

keyP' :: Bool -> Plurality -> Parser Key
keyP' addCompleter plurality = Key <$> strArgument (metavar name <> if addCompleter then completer (mkCompleter' keyCompleter) else mempty) where
  name = case plurality of
    Plural -> "KEYS..."
    Singular -> "KEY"

keyP :: Bool -> Parser Key
keyP = flip keyP' Singular
