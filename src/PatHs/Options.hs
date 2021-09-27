{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
module PatHs.Options (commandP) where

import           Options.Applicative
import           PatHs.Types

commandP :: ParserInfo SomeCommand
commandP = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Save often used directories like bookmarks"
  <> header "pat-hs - a terminal directory bookmark utility")

commandParser :: Parser SomeCommand
commandParser = subparser (
     command "save" (mkCommand saveP "Save bookmark")
  <> command "delete" (mkCommand deleteP "Delete bookmark")
  <> command "get" (mkCommand getP "Get bookmark")
  <> command "list" (mkCommand listP "List all bookmarks")
  )
  where
    mkCommand :: Parser (Command c) -> String -> ParserInfo SomeCommand
    mkCommand parser desc = info (SomeCommand <$> parser) $ progDesc desc

saveP :: Parser (Command 'Save)
saveP = CSave <$> keyP <*> (Value <$> argument str (metavar "VALUE"))

deleteP :: Parser (Command 'Delete)
deleteP = CDelete <$> keyP

getP :: Parser (Command 'Get)
getP = CGet <$> keyP

listP :: Parser (Command 'List)
listP = pure CList

keyP :: Parser Key
keyP = Key <$> argument str (metavar "KEY")
