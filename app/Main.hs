module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Options.Applicative (execParser)
import PatHs.Lib
import PatHs.Options (commandP)
import PatHs.Types
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)

main :: IO ()
main = do
  result <- runApp app
  case result of
    Left err -> do
      print err
      exitFailure
    Right _ -> pure ()

app :: AppM ()
app = do
  marks <- loadMarks
  currentDir <- Text.pack <$> liftIO getCurrentDirectory
  homeDir <- liftIO getHomeDirectory'
  (SomeCommand command) <- liftIO $ execParser (commandP homeDir $ unResolveToHomeDir homeDir currentDir)
  runPatHs marks command
