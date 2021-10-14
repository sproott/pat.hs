{-# LANGUAGE GADTs #-}

module PatHs.Options.Complete
  ( mkCompleter',
    keyCompleter,
    goPathCompleter,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (except)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
  ( Completer,
    bashCompleter,
    mkCompleter,
  )
import Options.Applicative.Types (Completer (runCompleter))
import PatHs.Lib
import PatHs.Lib.Command
import PatHs.Lib.Text (replacePrefix)
import PatHs.Parser (parse, splitGoPath)
import PatHs.Types
import System.FilePath.Text
  ( addTrailingPathSeparator,
    (</>),
  )
import System.IO.Error (catchIOError)

mkCompleter' :: MyCompleter -> Completer
mkCompleter' complete =
  mkCompleter $ \str -> fmap Text.unpack . fromRight [] <$> runApp (complete $ Text.pack str)

type MyCompleter = Text -> AppM [Text]

keyCompleter :: MyCompleter
keyCompleter str = do
  marks <- loadMarks
  (RTList marks) <- except $ list CList marks
  pure $ filter (Text.isPrefixOf str) $ unValidKey <$> Map.keys marks

goPathCompleter :: MyCompleter
goPathCompleter str = do
  (keyStr, goPathStr) <- except $ parse InvalidGoPath splitGoPath str
  marks <- loadMarks
  case keyStr of
    Nothing -> pure $ completeMarks $ Map.toList marks
    Just keyStr -> do
      (RTList marks) <- except $ list CList marks
      let goPath = GoPath (Key keyStr) goPathStr
      let matchingMarks = filterMarks (Text.isPrefixOf keyStr) marks
      let exactMatch =
            if length matchingMarks == 1
              then pure $ head matchingMarks
              else listToMaybe $ filterMarks (== keyStr) marks
      case (length matchingMarks == 1 || isJust (path goPath), exactMatch) of
        (True, Just mark) -> liftIO $ completeSingleMark mark goPath
        _ -> pure $ completeMarks matchingMarks
  where
    completeMarks matchingMarks = addTrailingPathSeparator . unValidKey . fst <$> matchingMarks
    filterMarks fn = filter (fn . unValidKey . fst) . Map.toList

completeSingleMark :: (ValidKey, Value) -> GoPath -> IO [Text]
completeSingleMark mark goPath =
  resolveDirs mark goPath
    `catchIOError` const (pure [])
  where
    key = unValidKey $ fst mark
    resolveDirs mark goPath = do
      homeDir <- getHomeDirectory'

      let value = unResolvedValue (resolveToHomeDir homeDir $ unValue (snd mark))
      let fullPath = case path goPath of
            Just goPathStr -> value </> goPathStr
            Nothing -> value

      dirs <- completeDirectory fullPath
      fmap (replacePrefix key value . addTrailingPathSeparator) <$> case dirs of
        [dir] -> do
          newCompletions <-
            completeDirectory $ addTrailingPathSeparator $ value </> dir
          pure $ dir : newCompletions
        dirs -> pure dirs

completeDirectory :: Text -> IO [Text]
completeDirectory =
  fmap (fmap Text.pack) . runCompleter (bashCompleter "directory") . Text.unpack
