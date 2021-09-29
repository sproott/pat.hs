{-# LANGUAGE GADTs #-}

module PatHs.Options.Complete where

import           Control.Monad.Trans.Except (except)
import           Data.Either                (fromRight)
import           Data.List                  (isPrefixOf)
import qualified Data.Map.Strict            as Map
import           Options.Applicative        (Completer, mkCompleter)
import           PatHs.Lib
import           PatHs.Lib.Command
import           PatHs.Types

mkCompleter' :: (String -> AppM [String]) -> Completer
mkCompleter' complete = mkCompleter $ \str -> fromRight [] <$> runApp (complete str)

keyCompleter :: Completer
keyCompleter = mkCompleter' complete
  where
    complete :: String -> AppM [String]
    complete str = do
      marks <- loadMarks
      (RTList marks) <- except $ list CList marks
      pure $ filter (isPrefixOf str) $ unValidKey <$> Map.keys marks
