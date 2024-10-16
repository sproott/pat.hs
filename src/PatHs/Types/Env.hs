module PatHs.Types.Env where

import PatHs.Prelude
import PatHs.Types

data Dirs = Dirs
  { dirHome :: HomeDir,
    dirConfig :: FilePath,
    dirCurrent :: FilePath
  }

newtype Env = Env
  { envIsStdoutInteractive :: Bool
  }
