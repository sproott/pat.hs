{-# LANGUAGE GADTs #-}

module PatHs.Options.Complete(mkCompleter', keyCompleter, goPathCompleter) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (except)
import           Data.Either                (fromRight)
import           Data.List                  (isPrefixOf)
import           Data.List.Extra            (notNull)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (listToMaybe)
import           Options.Applicative        (Completer, bashCompleter,
                                             mkCompleter)
import           Options.Applicative.Types  (Completer (runCompleter))
import           PatHs.Lib
import           PatHs.Lib.Command
import           PatHs.Types
import           System.FilePath            (addTrailingPathSeparator,
                                             makeRelative, (</>))
import           System.IO.Error            (catchIOError)

mkCompleter' :: MyCompleter -> Completer
mkCompleter' complete = mkCompleter $ \str -> fromRight [] <$> runApp (complete str)

type MyCompleter = String -> AppM [String]

keyCompleter :: MyCompleter
keyCompleter str = do
  marks <- loadMarks
  (RTList marks) <- except $ list CList marks
  pure $ filter (isPrefixOf str) $ unValidKey <$> Map.keys marks

goPathCompleter :: MyCompleter
goPathCompleter str = do
  let goPath = mkGoPath str
  if null (unKey $ key goPath) && notNull (path goPath) then pure [] else do
    marks <- loadMarks
    (RTList marks) <- except $ list CList marks
    let filterMarks fn = filter (fn . unValidKey . fst) $ Map.toList marks
    let matchingMarks = filterMarks $ isPrefixOf $ unKey $ key goPath
    let exactMatch = listToMaybe $ filterMarks (== unKey (key goPath))
    case (length matchingMarks == 1 || notNull (path goPath), exactMatch) of
      (True, Just mark) -> liftIO $ completeSingleMark mark goPath
      _   -> pure $ addTrailingPathSeparator . unValidKey . fst <$> matchingMarks

completeSingleMark :: (ValidKey, Value) -> GoPath -> IO [String]
completeSingleMark mark goPath = resolveDirs mark goPath `catchIOError` const (pure [])
  where
    key = unValidKey $ fst mark
    resolveDirs mark goPath = do
      homeDir <- getHomeDirectory'
      let value = unResolvedValue (resolveToHomeDir homeDir $ unValue (snd mark))
      dirs <- completeDirectory $ value <> path goPath
      fmap ((key </>) . makeRelative value) <$> case dirs of
        [dir] -> do
          newCompletions <- completeDirectory $ addTrailingPathSeparator $ value </> dir
          pure $ addTrailingPathSeparator dir : newCompletions
        dirs -> pure dirs

completeDirectory :: String -> IO [FilePath]
completeDirectory = runCompleter (bashCompleter "directory")
