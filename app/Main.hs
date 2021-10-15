module Main where

import qualified Data.Text as Text
import Options.Applicative (execParser)
import PatHs.Lib
import PatHs.Options
import PatHs.Render
import PatHs.Types
import Prettyprinter.Render.Terminal (putDoc)
import System.Directory (getCurrentDirectory)
import Prelude

main :: IO ()
main = do
  result <- runApp app
  case result of
    Left err -> do
      homeDir <- getHomeDirectory'
      putDoc $ wrapWithEmptyLines $ renderError homeDir err
      exitFailure
    Right _ -> pure ()

app :: AppM ()
app = do
  marks <- loadMarks
  currentDir <- Text.pack <$> liftIO getCurrentDirectory
  homeDir <- liftIO getHomeDirectory'
  (SomeCommand command) <- liftIO $ execParser (commandP homeDir $ unResolveToHomeDir homeDir currentDir)
  runPatHs marks command
