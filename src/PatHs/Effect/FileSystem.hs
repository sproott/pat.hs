{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.FileSystem where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8')
import Effectful
import Effectful.Dispatch.Dynamic
import PatHs.Prelude hiding (readFile, writeFile)
import qualified PatHs.Prelude as IO (readFileBS, writeFile)
import qualified System.Directory as IO (createDirectoryIfMissing)
import Effectful.TH

data FileSystem :: Effect where
  CreateDirectoryIfMissing :: Bool -> FilePath -> FileSystem m ()
  ReadFile :: FilePath -> FileSystem m (Maybe Text)
  WriteFile :: FilePath -> Text -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

makeEffect ''FileSystem

runFileSystemIO :: IOE :> es => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  CreateDirectoryIfMissing parents dir -> liftIO $ IO.createDirectoryIfMissing parents dir
  ReadFile path -> liftIO $ fmap join $ safeIOMaybe $ fmap rightToMaybe $ T.decodeUtf8' <$> IO.readFileBS path
  WriteFile path contents -> liftIO $ IO.writeFile path $ T.unpack contents
