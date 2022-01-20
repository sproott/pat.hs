module Main where

import Effectful
import qualified Effectful.Reader.Static as Reader
import Options.Applicative (execParser)
import qualified PatHs.Effect.Error as Error
import qualified PatHs.Effect.FileSystem as FS
import qualified PatHs.Effect.Output as Output
import PatHs.Lib
import PatHs.Options
import PatHs.Prelude
import PatHs.Render
import PatHs.Types
import PatHs.Types.Env
import Prettyprinter.Render.Terminal (putDoc)

main :: IO ()
main = do
  result <- app
  case result of
    Left err -> do
      homeDir <- getHomeDirectory'
      putDoc $ wrapWithEmptyLines $ renderError homeDir err
      exitFailure
    Right _ -> pure ()

app :: IO (Either AppError ())
app = do
  dirs <- dirsIO
  env <- envIO
  (SomeCommand command) <- execParser commandP
  runPatHs command
    & runWithMarks
    & FS.runFileSystemIO
    & Output.runOutputIO
    & Reader.runReader dirs
    & Reader.runReader env
    & Error.runError_
    & runEff
