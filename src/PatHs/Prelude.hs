module PatHs.Prelude (module Relude, module Protolude.Safe, safeIOMaybe) where

import Protolude.Safe
import Relude hiding (Reader)
import System.IO.Error (catchIOError)

safeIOMaybe :: IO a -> IO (Maybe a)
safeIOMaybe m = (Just <$> m) `catchIOError` const (pure Nothing)
