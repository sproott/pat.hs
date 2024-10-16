{-# LANGUAGE BangPatterns #-}

module PatHs.Lib where

import qualified Data.Map.Strict as Map
import Effectful
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as Reader
import PatHs.Config
import PatHs.Effect.Error (Error)
import qualified PatHs.Effect.Error as Error
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
import System.Directory (getCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath ((</>))
import System.Posix (queryTerminal, stdOutput)

loadMarks :: '[Error AppError, FileSystem, Reader Dirs] :>> es => Eff es Marks
loadMarks = do
  config <- loadConfig
  Error.fromEither $ Map.fromList <$> convertKeys validateKey config

loadMarksIO :: '[IOE, Reader Dirs] :>> es => Eff es (Either AppError Marks)
loadMarksIO = loadMarks & FS.runFileSystemIO & Error.runErrorNoCallStack

getConfigPath :: Reader Dirs :> es => Eff es FilePath
getConfigPath = (</> ".bookmarks") <$> Reader.asks dirConfig

loadConfig :: '[Error AppError, FileSystem, Reader Dirs] :>> es => Eff es Config
loadConfig = do
  configDir <- Reader.asks dirConfig
  FS.createDirectoryIfMissing True configDir
  configPath <- getConfigPath
  !contents <- FS.readFile configPath
  parseConfig $ fromMaybe "" contents

saveConfig :: '[FileSystem, Reader Dirs] :>> es => Marks -> Eff es ()
saveConfig marks = do
  configPath <- getConfigPath
  FS.writeFile configPath $ marksToConfigString marks

parseConfig :: '[Error AppError, Reader Dirs] :>> es => Text -> Eff es Config
parseConfig input = do
  configParser' <- configParser
  Error.fromEither $ parse (ConfigError CEInvalid) configParser' input

convertKeys :: (a -> Either e a') -> [(a, b)] -> Either e [(a', b)]
convertKeys f = traverse $ bitraverse f pure

runPatHs :: '[Error AppError, FileSystem, Output Text, Reader Dirs, Reader Env, Reader Marks] :>> es => Command c -> Eff es ()
runPatHs command@CSave {} = execSave command >>= saveAndPrintMarks
runPatHs command@CDelete {} = execDelete command >>= saveAndPrintMarks
runPatHs command@CRename {} = execRename command >>= saveAndPrintMarks
runPatHs command@CGet {} = do
  value <- execGet command
  homeDir <- Reader.asks dirHome
  interactive <- Reader.asks envIsStdoutInteractive
  ( if interactive
      then Output.putAnsiDoc . renderResolvedValue
      else Output.output . unResolvedValue
    )
    $ resolveToHomeDir homeDir $ unValue value
runPatHs command@CGo {} = do
  value <- execGo command
  Output.putStrLn $ unResolvedValue value
runPatHs command@CList = do
  marks <- execList command
  resolvedMarks <- resolveMarks marks
  Output.putAnsiDoc $ renderMarks resolvedMarks

saveAndPrintMarks :: '[FileSystem, Output Text, Reader Dirs] :>> es => Marks -> Eff es ()
saveAndPrintMarks marks = do
  saveConfig marks
  resolvedMarks <- resolveMarks marks
  Output.putAnsiDoc $ renderMarks resolvedMarks

resolveMarks :: Reader Dirs :> es => Marks -> Eff es ResolvedMarks
resolveMarks marks = do
  homeDir <- Reader.asks dirHome
  pure $ Map.map (resolveToHomeDir homeDir . unValue) marks

runWithMarks :: '[Error AppError, FileSystem, Reader Dirs] :>> es => Eff (Reader Marks : es) a -> Eff es a
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
