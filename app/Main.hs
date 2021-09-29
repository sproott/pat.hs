module Main where

import           Control.Monad.IO.Class (liftIO)
import           Options.Applicative    (execParser)
import           PatHs.Lib
import           PatHs.Options          (commandP)
import           PatHs.Types
import           System.Directory       (getCurrentDirectory)
import           System.Exit            (exitFailure)

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
  currentDirectory <- liftIO getCurrentDirectory
  (SomeCommand command) <- liftIO $ execParser (commandP currentDirectory)
  runPatHs marks command
