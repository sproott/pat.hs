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
goPathCompleter str =
  if T.null str
    then do
      marks <- execList CList
      pure $ completeMarks $ Map.toList marks
    else do
      (keyStr, goPathStr) <- Error.fromEither $ parse InvalidGoPath parseGoPath str
      marks <- execList CList
      let goPath = GoPath (Key keyStr) goPathStr
      let matchingMarks = filterMarks (T.isPrefixOf keyStr) marks
      let exactMatch = viaNonEmpty head matchingMarks
      case (length matchingMarks == 1 || isJust (gpPath goPath), exactMatch) of -- either there is only one mark with the given prefix, or the user has written "/" after the mark
        (True, Just mark) -> completeSingleMark mark goPath
        _ -> pure $ completeMarks matchingMarks
  where
    completeMarks matchingMarks = addTrailingPathSeparator . unValidKey . fst <$> matchingMarks
    filterMarks fn = filter (fn . unValidKey . fst) . Map.toList

completeSingleMark :: (Complete :> es, Reader Dirs :> es) => (ValidKey, Value) -> GoPath -> Eff es [Text]
completeSingleMark mark goPath = do
  let key = unValidKey $ fst mark
  homeDir <- Reader.asks dirHome

  let value = unResolvedValue (resolveToHomeDir homeDir $ unValue (snd mark)) -- the mark's value, resolved to the home dir

  let path = (addTrailingPathSeparator value </>) $ fromMaybe "" $ gpPath goPath -- the path to complete
  directChildDirs <- Complete.completeDirectory path -- the direct child directories of the path
  
  dirs <- case directChildDirs of
    [dir] -> -- if there is only one direct child directory
      do 
        newCompletions <- Complete.completeDirectory $ addTrailingPathSeparator dir
        pure $ (if isJust (gpPath goPath) then id else (value : )) $ dir : newCompletions
    [] -> pure $ if isNothing (gpPath goPath) then [value] else []
    _ -> pure directChildDirs
  
  pure $ (replacePrefix value key . addTrailingPathSeparator <$> dirs)
