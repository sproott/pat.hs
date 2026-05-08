{-# LANGUAGE ScopedTypeVariables #-}

module PatHs.Options.Complete
  ( mkCompleter',
    keyCompleter,
    goPathCompleter,
    goPathCompleterIO,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Effectful
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as Reader
import Options.Applicative (Completer, mkCompleter)
import PatHs.Effect.Complete (Complete)
import qualified PatHs.Effect.Complete as Complete
import PatHs.Effect.Error (Error)
import qualified PatHs.Effect.Error as Error
import PatHs.Effect.FileSystem (FileSystem)
import qualified PatHs.Effect.FileSystem as FS
import PatHs.Lib
import PatHs.Lib.Command
import PatHs.Lib.Text (replacePrefix)
import PatHs.Parser (parse, parseGoPath)
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import System.FilePath.Text
  ( addTrailingPathSeparator,
    (</>),
  )

type MyCompleter es = Text -> Eff es [Text]

-- | Represents the parsed completion input: key and optional path component
data CompletionInput = CompletionInput
  { ciKey :: Text,
    ciPath :: Maybe Text
  }
  deriving (Eq, Show)

-- | Represents a mark with its resolved value
data ResolvedMark = ResolvedMark
  { rmKey :: ValidKey,
    rmResolvedValue :: Text
  }
  deriving (Eq, Show)

-- ============================================================================
-- Phase 1: Mark Filtering & Input Parsing
-- ============================================================================

-- | Parse completion input into key and optional path component
parseCompletionInput :: Text -> Either AppError CompletionInput
parseCompletionInput str = do
  (keyStr, pathStr) <- parse InvalidGoPath parseGoPath str
  pure $ CompletionInput keyStr pathStr

-- | Filter marks by key prefix, returning matching (key, value) pairs
filterMarksByPrefix :: Text -> Marks -> [(ValidKey, Value)]
filterMarksByPrefix prefix marks =
  filter (T.isPrefixOf prefix . unValidKey . fst) $ Map.toList marks

-- | Format mark keys for display (with trailing slash)
formatMarkKeys :: [(ValidKey, Value)] -> [Text]
formatMarkKeys marks =
  addTrailingPathSeparator . unValidKey . fst <$> marks

-- | Decide if we should enter path completion mode for this input
--   We enter path mode if: exactly one mark matches OR user has typed a path separator
shouldEnterPathMode :: CompletionInput -> [(ValidKey, Value)] -> Bool
shouldEnterPathMode input matches =
  length matches == 1 || isJust (ciPath input)

-- ============================================================================
-- Phase 2: Path Resolution & Directory Completion
-- ============================================================================

-- | Resolve a mark's value to its full path
resolveMarkValue :: HomeDir -> (ValidKey, Value) -> ResolvedMark
resolveMarkValue homeDir (key, value) =
  let resolvedValue = unResolvedValue (resolveToHomeDir homeDir (unValue value))
   in ResolvedMark key resolvedValue

-- | Build the completion path from a resolved mark and optional path component
buildCompletionPath :: ResolvedMark -> Maybe Text -> Text
buildCompletionPath mark pathComponent =
  addTrailingPathSeparator (rmResolvedValue mark)
    </> fromMaybe "" pathComponent

-- | Determine which directories to include in completions based on cascading logic
--   - If 0 children and no explicit path: return mark value (user can select it)
--   - If 1 child: recursively complete it (cascade completion)
--   - If 2+ children: return all (optionally prefixed with mark value if no path given)
selectCompletionDirs ::
  (Complete :> es) =>
  ResolvedMark ->
  Maybe Text ->
  [Text] ->
  Eff es [Text]
selectCompletionDirs mark pathComponent dirs = case dirs of
  [] ->
    -- No children found
    if isNothing pathComponent
      then pure [rmResolvedValue mark] -- No path given: return mark value
      else pure [] -- Path given but doesn't exist: return nothing
  [singleDir] ->
    -- Exactly one child: cascade to next level
    do
      nextDirs <- Complete.completeDirectory (addTrailingPathSeparator singleDir)
      let prefix = if isNothing pathComponent then [rmResolvedValue mark] else []
      pure $ prefix <> [singleDir] <> nextDirs
  _ ->
    -- Multiple children: include mark value if no path given, then list all dirs
    let prefix = if isNothing pathComponent then [rmResolvedValue mark] else []
     in pure $ prefix <> dirs

-- | Format completion results: replace resolved path with key and add trailing slashes
--   When pathComponent is provided (user typed a path): add trailing slashes to everything
--   When pathComponent is None (just mark key): only add trailing slashes to mark value and cascaded results
formatCompletionResults ::
  ResolvedMark ->
  Maybe Text ->
  [Text] ->
  [Text]
formatCompletionResults mark pathComponent dirs =
  map formatDir dirs
  where
    -- When path component provided, always add trailing slashes
    -- When not provided, only add to mark value (not to multiple sibling dirs)
    childDirs = filter (/= rmResolvedValue mark) dirs
    hasMultipleChildren = length childDirs > 1
    shouldAddSlashes = isJust pathComponent || not hasMultipleChildren

    formatDir dir
      | dir == rmResolvedValue mark =
          addTrailingPathSeparator (unValidKey (rmKey mark))
      | shouldAddSlashes =
          addTrailingPathSeparator
            $ replacePrefix (rmResolvedValue mark) (unValidKey (rmKey mark)) dir
      | otherwise =
          replacePrefix (rmResolvedValue mark) (unValidKey (rmKey mark)) dir

-- ============================================================================
-- Main Completers
-- ============================================================================

mkCompleter' :: MyCompleter '[Reader Marks, FileSystem, Reader Dirs, Error AppError, IOE] -> Completer
mkCompleter' completer = mkCompleter $ \str -> fmap (fmap T.unpack) $ runCompleterIO completer $ T.pack str

runCompleterIO :: MyCompleter '[Reader Marks, FileSystem, Reader Dirs, Error AppError, IOE] -> Text -> IO [Text]
runCompleterIO completer str = do
  dirs <- dirsIO
  completions <-
    completer str
      & runWithMarks
      & FS.runFileSystemIO
      & Reader.runReader dirs
      & Error.runErrorNoCallStack @AppError
      & runEff
  pure $ fromRight [] completions

keyCompleter :: (Reader Marks :> es) => MyCompleter es
keyCompleter str = do
  marks <- execList CList
  pure $ filter (T.isPrefixOf str) $ unValidKey <$> Map.keys marks

goPathCompleterIO :: (IOE :> es, Reader Marks :> es) => MyCompleter es
goPathCompleterIO str = do
  dirs <- liftIO dirsIO
  completions <- goPathCompleter str & Complete.runCompleteIO & Reader.runReader dirs & Error.runErrorNoCallStack @AppError
  pure $ fromRight [] completions

goPathCompleter :: (Complete :> es, Error AppError :> es, Reader Dirs :> es, Reader Marks :> es) => MyCompleter es
goPathCompleter str
  | T.null str = completeAllMarks
  | otherwise = completePathWithMatches str
  where
    -- Case 1: Empty input → list all marks
    completeAllMarks = do
      marks <- execList CList
      pure $ formatMarkKeys $ Map.toList marks

    -- Case 2: Non-empty input → parse, filter, and decide whether to complete path
    completePathWithMatches str' = do
      input <- Error.fromEither $ parseCompletionInput str'
      marks <- execList CList
      let matchingMarks = filterMarksByPrefix (ciKey input) marks
      if shouldEnterPathMode input matchingMarks
        then case viaNonEmpty head matchingMarks of
          Just mark -> completeSingleMark mark (ciPath input)
          Nothing -> pure []
        else pure $ formatMarkKeys matchingMarks

-- | Complete paths within a single mark
--   Strategy:
--     1. Resolve the mark's value to its actual path
--     2. Build the target path (resolved value + any path component)
--     3. Get child directories
--     4. Apply cascading logic: if 1 child, recursively complete it
--     5. Format results using mark key instead of resolved path
completeSingleMark :: (Complete :> es, Reader Dirs :> es) => (ValidKey, Value) -> Maybe Text -> Eff es [Text]
completeSingleMark mark pathComponent = do
  homeDir <- Reader.asks dirHome
  let resolved = resolveMarkValue homeDir mark
  let targetPath = buildCompletionPath resolved pathComponent
  childDirs <- Complete.completeDirectory targetPath
  selectedDirs <- selectCompletionDirs resolved pathComponent childDirs
  pure $ formatCompletionResults resolved pathComponent selectedDirs
