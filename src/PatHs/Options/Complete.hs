module PatHs.Options.Complete
  ( mkCompleter',
    keyCompleter,
    goPathCompleter,
    goPathCompleterIO,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Options.Applicative (Completer, mkCompleter)
import PatHs.Effect.Complete (Complete)
import qualified PatHs.Effect.Complete as Complete
import PatHs.Effect.FileSystem (FileSystem)
import qualified PatHs.Effect.FileSystem as FS
import PatHs.Lib
import PatHs.Lib.Command
import PatHs.Lib.Text (replacePrefix)
import PatHs.Parser (parse, splitGoPath)
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import Polysemy (Embed, Member, Members, Sem, embed, runM)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import System.FilePath.Text
  ( addTrailingPathSeparator,
    (</>),
  )

type MyCompleter r = Text -> Sem r [Text]

mkCompleter' :: MyCompleter '[Reader Marks, FileSystem, Reader Dirs, Error AppError, Embed IO] -> Completer
mkCompleter' completer = mkCompleter $ \str -> fmap (fmap T.unpack) $ runCompleterIO completer $ T.pack str

runCompleterIO :: MyCompleter '[Reader Marks, FileSystem, Reader Dirs, Error AppError, Embed IO] -> Text -> IO [Text]
runCompleterIO completer str = do
  dirs <- dirsIO
  completions <-
    completer str
      & runWithMarks
      & FS.runFileSystemIO
      & Reader.runReader dirs
      & Error.runError @AppError
      & runM
  pure $ fromRight [] completions

keyCompleter :: Member (Reader Marks) r => MyCompleter r
keyCompleter str = do
  marks <- execList CList
  pure $ filter (T.isPrefixOf str) $ unValidKey <$> Map.keys marks

goPathCompleterIO :: Members '[Embed IO, Reader Marks] r => MyCompleter r
goPathCompleterIO str = do
  dirs <- embed dirsIO
  completions <- goPathCompleter str & Complete.runCompleteIO & Reader.runReader dirs & Error.runError
  pure $ fromRight [] completions

goPathCompleter :: Members '[Complete, Error AppError, Reader Dirs, Reader Marks] r => MyCompleter r
goPathCompleter str = do
  (keyStr, goPathStr) <- Error.fromEither $ parse InvalidGoPath splitGoPath str
  marks <- execList CList
  let goPath = GoPath (Key keyStr) goPathStr
  case keyStr of
    "" -> case path goPath of
      Nothing -> pure $ completeMarks $ Map.toList marks
      Just _ -> pure []
    _ -> do
      let matchingMarks = filterMarks (T.isPrefixOf keyStr) marks
      let exactMatch = viaNonEmpty head matchingMarks <|> listToMaybe (filterMarks (== keyStr) marks)
      case (length matchingMarks == 1 || isJust (path goPath), exactMatch) of
        (True, Just mark) -> completeSingleMark mark goPath
        _ -> pure $ completeMarks matchingMarks
  where
    completeMarks matchingMarks = addTrailingPathSeparator . unValidKey . fst <$> matchingMarks
    filterMarks fn = filter (fn . unValidKey . fst) . Map.toList

completeSingleMark :: Members '[Complete, Reader Dirs] r => (ValidKey, Value) -> GoPath -> Sem r [Text]
completeSingleMark mark goPath = do
  let key = unValidKey $ fst mark
  homeDir <- dirHome <$> Reader.ask

  let value = unResolvedValue (resolveToHomeDir homeDir $ unValue (snd mark))
  let fullPath = case path goPath of
        Just goPathStr -> value </> goPathStr
        Nothing -> value

  dirs <- Complete.completeDirectory fullPath
  fmap (replacePrefix value key . addTrailingPathSeparator) <$> case dirs of
    [dir] -> do
      newCompletions <-
        Complete.completeDirectory $ addTrailingPathSeparator $ value </> dir
      pure $ dir : newCompletions
    dirs -> pure dirs
