{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.FileSystem where

import qualified Data.Text as T
import PatHs.Prelude
import qualified PatHs.Prelude as IO (readFile, writeFile)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import qualified System.Directory as IO (createDirectoryIfMissing)

data FileSystem m a where
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem m ()
  ReadFile :: FilePath -> FileSystem m (Maybe Text)
  WriteFile :: FilePath -> Text -> FileSystem m ()

makeSem ''FileSystem

runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
  CreateDirectoryIfMissing parents dir -> embed $ IO.createDirectoryIfMissing parents dir
  ReadFile path -> embed $ safeIOMaybe $ T.pack <$> IO.readFile path
  WriteFile path contents -> embed $ IO.writeFile path $ T.unpack contents
