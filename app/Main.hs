module Main where

import qualified Data.Text as T
import Options.Applicative (execParser)
import qualified PatHs.Effect.FileSystem as FS
import qualified PatHs.Effect.Output as Output
import PatHs.Lib
import PatHs.Options
import PatHs.Prelude
import PatHs.Render
import PatHs.Types
import PatHs.Types.Env
import Polysemy (runM)
import qualified Polysemy.Error as Error
import qualified Polysemy.Reader as Reader
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
  let homeDir = dirHome dirs
  (SomeCommand command) <- execParser (commandP $ unResolveToHomeDir homeDir (T.pack $ dirCurrent dirs))
  runPatHs command
    & runWithMarks
    & FS.runFileSystemIO
    & Output.runOutputIO
    & Reader.runReader dirs
    & Reader.runReader env
    & Error.runError
    & runM
