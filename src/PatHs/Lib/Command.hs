{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module PatHs.Lib.Command where

import Data.Either.Extra (maybeToEither)
import qualified Data.Map.Strict as Map
import PatHs.Types
import System.FilePath.Text (dropTrailingPathSeparator, (</>))
import Prelude

type ExecCommand (c :: CommandType) = Command c -> Marks -> Either Error (ReturnType c)

execSave :: ExecCommand Save
execSave (CSave key value) marks = do
  validKey <- validateKey key
  case Map.lookup validKey marks of
    Just value -> Left $ AlreadyExists key value
    Nothing -> pure $ RTSave $ Map.insert validKey value marks

execDelete :: ExecCommand Delete
execDelete (CDelete key) marks = do
  execGet' (CGet key) marks
  validKey <- validateKey key
  pure $ RTDelete $ Map.delete validKey marks

execRename :: ExecCommand Rename
execRename (CRename key newKey) marks = do
  value <- execGet' (CGet key) marks
  validKey <- validateKey key
  validNewKey <- validateKey newKey
  pure $ RTRename $ Map.insert validNewKey value $ Map.delete validKey marks

execGet :: ExecCommand Get
execGet command marks = RTGet <$> execGet' command marks

execGet' :: Command Get -> Marks -> Either Error Value
execGet' (CGet key) marks = do
  validKey <- validateKey key
  case Map.lookup validKey marks of
    Nothing -> Left $ NotExists key
    Just value -> pure value

execGo :: ExecCommand Go
execGo (CGo homeDir goPath) marks = do
  goPath <- maybeToEither InvalidGoPath goPath
  value <- execGet' (CGet $ Key (dropTrailingPathSeparator $ unKey $ key goPath)) marks
  pure $ RTGo $ resolveToHomeDir homeDir $ unValue value </> fromMaybe "" (path goPath)

execList :: ExecCommand List
execList CList = pure . RTList

runCommand :: ExecCommand c
runCommand command@CSave {} = execSave command
runCommand command@CDelete {} = execDelete command
runCommand command@CRename {} = execRename command
runCommand command@CGet {} = execGet command
runCommand command@CGo {} = execGo command
runCommand command@CList = execList command
