{-# LANGUAGE GADTs #-}

module PatHs.Options.Complete
  ( mkCompleter'
  , keyCompleter
  , goPathCompleter
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (except)
import           Data.Either                (fromRight)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Options.Applicative        (Completer, bashCompleter,
                                             mkCompleter)
import           Options.Applicative.Types  (Completer (runCompleter))
import           PatHs.Lib
import           PatHs.Lib.Command
import           PatHs.Types
import           System.FilePath.Text       (addTrailingPathSeparator,
                                             makeRelative, (</>))
import           System.IO.Error            (catchIOError)

mkCompleter' :: MyCompleter -> Completer
mkCompleter' complete =
  mkCompleter $ \str -> fmap Text.unpack . fromRight [] <$> runApp (complete $ Text.pack str)

type MyCompleter = Text -> AppM [Text]

keyCompleter :: MyCompleter
keyCompleter str = do
  marks          <- loadMarks
  (RTList marks) <- except $ list CList marks
  pure $ filter (Text.isPrefixOf str) $ unValidKey <$> Map.keys marks

goPathCompleter :: MyCompleter
goPathCompleter str = if Text.null (unKey $ key goPath) && not (Text.null (path goPath))
  then pure []
  else do
    marks          <- loadMarks
    (RTList marks) <- except $ list CList marks
    let matchingMarks = filterMarks (Text.isPrefixOf $ unKey $ key goPath) marks
    let exactMatch    = listToMaybe $ filterMarks (== unKey (key goPath)) marks
    case (length matchingMarks == 1 || not (Text.null (path goPath)), exactMatch) of
      (True, Just mark) -> liftIO $ completeSingleMark mark goPath
      _ -> pure $ addTrailingPathSeparator . unValidKey . fst <$> matchingMarks
 where
  goPath = mkGoPath str
  filterMarks fn = filter (fn . unValidKey . fst) . Map.toList

completeSingleMark :: (ValidKey, Value) -> GoPath -> IO [Text]
completeSingleMark mark goPath = resolveDirs mark goPath
  `catchIOError` const (pure [])
 where
  key = unValidKey $ fst mark
  resolveDirs mark goPath = do
    homeDir <- getHomeDirectory'
    let value = unResolvedValue (resolveToHomeDir homeDir $ unValue (snd mark))
    dirs <- completeDirectory $ value <> path goPath
    fmap ((key </>) . makeRelative value) <$> case dirs of
      [dir] -> do
        newCompletions <-
          completeDirectory $ addTrailingPathSeparator $ value </> dir
        pure $ addTrailingPathSeparator dir : newCompletions
      dirs -> pure dirs

completeDirectory :: Text -> IO [Text]
completeDirectory =
  fmap (fmap Text.pack) . runCompleter (bashCompleter "directory") . Text.unpack
