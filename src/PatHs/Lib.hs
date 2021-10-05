{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module PatHs.Lib where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except)
import Data.Bitraversable (bitraverse)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import PatHs.Config
import PatHs.Lib.Command
import PatHs.Parser
import PatHs.Render
import PatHs.Types
import Prettyprinter.Render.Terminal (putDoc)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import System.Posix (queryTerminal, stdOutput)

loadMarks :: AppM Marks
loadMarks = do
  config <- loadConfig
  except $ Map.fromList <$> convertKeys validateKey config

getConfigDir :: IO FilePath
getConfigDir = getUserDataDir "paths"

getConfigPath :: IO FilePath
getConfigPath = do
  dir <- getConfigDir
  pure $ dir </> ".bookmarks"

loadConfig :: AppM Config
loadConfig = do
  let safeIO' = safeIO (ConfigError CERead)
  homeDir <- safeIO' getHomeDirectory'
  contents <- safeIO' $ do
    createDirectoryIfMissing True =<< getConfigDir
    configPath <- getConfigPath
    !contents <- TextIO.readFile configPath `catchIOError` const (pure "")
    pure contents
  except $ parseConfig homeDir contents

saveConfig :: Marks -> AppM ()
saveConfig marks = safeIO (ConfigError CEWrite) $ do
  homeDir <- getHomeDirectory'
  configPath <- getConfigPath
  TextIO.writeFile configPath $ marksToConfigString marks

safeIO :: Error -> IO a -> AppM a
safeIO err action = ExceptT $ (Right <$> action) `catchIOError` const (pure $ Left err)

parseConfig :: HomeDir -> Text -> Either Error Config
parseConfig homeDir = parse (ConfigError CEInvalid) (configParser homeDir)

convertKeys :: (a -> Either e a') -> [(a, b)] -> Either e [(a', b)]
convertKeys f = traverse $ bitraverse f pure

runPatHs :: Marks -> Command c -> AppM ()
runPatHs marks command = do
  result <- except $ runCommand command marks
  consumeResult command result

consumeResult :: Command c -> ReturnType c -> AppM ()
consumeResult _ (RTSave marks) = saveConfig marks
consumeResult _ (RTDelete marks) = saveConfig marks
consumeResult _ (RTGet value) = liftIO $ do
  homeDir <- getHomeDirectory'
  interactive <- isInteractive
  ( if interactive
      then putDoc . renderResolvedValue
      else putStr . Text.unpack . unResolvedValue
    )
    $ resolveToHomeDir homeDir $ unValue value
consumeResult _ (RTGo value) = liftIO $ do
  homeDir <- getHomeDirectory'
  TextIO.putStrLn $ unResolvedValue value
consumeResult _ (RTList marks) = liftIO $ do
  homeDir <- getHomeDirectory'
  putDoc $ renderMarks $ resolveMarks homeDir marks

showMarks :: ResolvedMarks -> [Text]
showMarks marks = uncurry printTuple <$> Map.toList marks
  where
    printTuple validKey resolvedValue = unValidKey validKey <> "    " <> unResolvedValue resolvedValue

resolveMarks :: HomeDir -> Marks -> ResolvedMarks
resolveMarks homeDir = Map.map (resolveToHomeDir homeDir . unValue)

isInteractive :: IO Bool
isInteractive = queryTerminal stdOutput
