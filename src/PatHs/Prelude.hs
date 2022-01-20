module PatHs.Prelude (module Relude, safeIOMaybe, maximumMay) where

import Relude hiding (Reader)
import System.IO.Error (catchIOError)

safeIOMaybe :: IO a -> IO (Maybe a)
safeIOMaybe m = (Just <$> m) `catchIOError` const (pure Nothing)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay = viaNonEmpty last . sort
