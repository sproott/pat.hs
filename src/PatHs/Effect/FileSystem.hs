{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.FileSystem where

import qualified Data.Text as T
import Effectful
import PatHs.Prelude
import qualified PatHs.Prelude as IO (readFile, writeFile)
import qualified System.Directory as IO (createDirectoryIfMissing)

data FileSystem :: Effect where
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem m ()
  ReadFile :: FilePath -> FileSystem m (Maybe Text)
  WriteFile :: FilePath -> Text -> FileSystem m ()

type instance DispatchOf FileSystem = 'Dynamic

createDirectoryIfMissing :: (FileSystem :> es) => Bool -> FilePath -> Eff es ()
createDirectoryIfMissing parents = send . CreateDirectoryIfMissing parents

readFile :: (FileSystem :> es) => FilePath -> Eff es (Maybe Text)
readFile = send . ReadFile

writeFile :: (FileSystem :> es) => FilePath -> Text -> Eff es ()
writeFile path = send . WriteFile path

runFileSystemIO :: IOE :> es => Eff (FileSystem ': es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  CreateDirectoryIfMissing parents dir -> liftIO $ IO.createDirectoryIfMissing parents dir
  ReadFile path -> liftIO $ safeIOMaybe $ T.pack <$> IO.readFile path
  WriteFile path contents -> liftIO $ IO.writeFile path $ T.unpack contents
