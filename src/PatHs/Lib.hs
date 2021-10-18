{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module PatHs.Lib where

import qualified Data.Map.Strict as Map
import PatHs.Config
import PatHs.Effect.FileSystem (FileSystem)
import qualified PatHs.Effect.FileSystem as FS
import PatHs.Effect.Output (Output)
import qualified PatHs.Effect.Output as Output
import PatHs.Lib.Command
import PatHs.Parser
import PatHs.Prelude
import PatHs.Render
import PatHs.Types
import PatHs.Types.Env
import Polysemy (Embed, Member, Members, Sem)
import Polysemy.Error (Error, runError)
import qualified Polysemy.Error as Error
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import System.Directory (getCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath ((</>))
import System.Posix (queryTerminal, stdOutput)

loadMarks :: Members '[Error AppError, FileSystem, Reader Dirs] r => Sem r Marks
loadMarks = do
  config <- loadConfig
  Error.fromEither $ Map.fromList <$> convertKeys validateKey config

loadMarksIO :: Members '[Embed IO, Reader Dirs] r => Sem r (Either AppError Marks)
loadMarksIO = loadMarks & FS.runFileSystemIO & runError

getConfigPath :: Member (Reader Dirs) r => Sem r FilePath
getConfigPath = (</> ".bookmarks") . dirConfig <$> Reader.ask

loadConfig :: Members '[Error AppError, FileSystem, Reader Dirs] r => Sem r Config
loadConfig = do
  configDir <- dirConfig <$> Reader.ask
  FS.createDirectoryIfMissing True configDir
  configPath <- getConfigPath
  !contents <- FS.readFile configPath
  parseConfig contents

saveConfig :: Members '[FileSystem, Reader Dirs] r => Marks -> Sem r ()
saveConfig marks = do
  homeDir <- dirHome <$> Reader.ask
  configPath <- dirConfig <$> Reader.ask
  FS.writeFile configPath $ marksToConfigString marks

parseConfig :: Members '[Error AppError, Reader Dirs] r => Text -> Sem r Config
parseConfig input = do
  configParser' <- configParser
  Error.fromEither $ parse (ConfigError CEInvalid) configParser' input

convertKeys :: (a -> Either e a') -> [(a, b)] -> Either e [(a', b)]
convertKeys f = traverse $ bitraverse f pure

runPatHs :: Members '[Error AppError, FileSystem, Output, Reader Dirs, Reader Env, Reader Marks] r => Command c -> Sem r ()
runPatHs command@CSave {} = execSave command >>= saveConfig
runPatHs command@CDelete {} = execDelete command >>= saveConfig
runPatHs command@CRename {} = execRename command >>= saveConfig
runPatHs command@CGet {} = do
  value <- execGet command
  homeDir <- dirHome <$> Reader.ask
  interactive <- envIsStdoutInteractive <$> Reader.ask
  ( if interactive
      then Output.putAnsiDoc . renderResolvedValue
      else Output.putStr . unResolvedValue
    )
    $ resolveToHomeDir homeDir $ unValue value
runPatHs command@CGo {} = do
  value <- execGo command
  Output.putStrLn $ unResolvedValue value
runPatHs command@CList = do
  homeDir <- dirHome <$> Reader.ask
  marks <- execList command
  resolvedMarks <- resolveMarks marks
  Output.putAnsiDoc $ renderMarks resolvedMarks

resolveMarks :: Member (Reader Dirs) r => Marks -> Sem r ResolvedMarks
resolveMarks marks = do
  homeDir <- dirHome <$> Reader.ask
  pure $ Map.map (resolveToHomeDir homeDir . unValue) marks

runWithMarks :: Members '[Error AppError, FileSystem, Reader Dirs] r => Sem (Reader Marks : r) a -> Sem r a
runWithMarks f = do
  marks <- loadMarks
  f & Reader.runReader marks

getConfigDir :: IO FilePath
getConfigDir = getUserDataDir "paths"

dirsIO :: IO Dirs
dirsIO = Dirs <$> getHomeDirectory' <*> getConfigDir <*> getCurrentDirectory

envIO :: IO Env
envIO = Env <$> isInteractive

isInteractive :: IO Bool
isInteractive = queryTerminal stdOutput
