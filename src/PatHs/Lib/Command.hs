{-# LANGUAGE KindSignatures #-}

module PatHs.Lib.Command where

import qualified Data.Map.Strict as Map
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import System.FilePath.Text ((</>))

type ExecCommand (c :: CommandType) a r = Members '[Error AppError, Reader Marks] r => Command c -> Sem r a

validateKey' :: Member (Error AppError) r => Key -> Sem r ValidKey
validateKey' = Error.fromEither . validateKey

execSave :: ExecCommand Save Marks r
execSave (CSave key value) = do
  marks <- Reader.ask
  validKey <- validateKey' key
  case Map.lookup validKey marks of
    Just val -> Error.throw $ AlreadyExists key val
    Nothing -> pure $ Map.insert validKey value marks

execDelete :: ExecCommand Delete Marks r
execDelete (CDelete key) = do
  marks <- Reader.ask
  _ <- execGet (CGet key)
  validKey <- validateKey' key
  pure $ Map.delete validKey marks

execRename :: ExecCommand Rename Marks r
execRename (CRename key newKey) = do
  marks <- Reader.ask
  value <- execGet (CGet key)
  validKey <- validateKey' key
  validNewKey <- validateKey' newKey
  pure $ Map.insert validNewKey value $ Map.delete validKey marks

execGet :: ExecCommand Get Value r
execGet (CGet key) = do
  marks <- Reader.ask
  validKey <- validateKey' key
  case Map.lookup validKey marks of
    Nothing -> Error.throw $ NotExists key
    Just value -> pure value

execGo :: Member (Reader Dirs) r => ExecCommand Go ResolvedValue r
execGo (CGo maybeGoPath) = do
  homeDir <- Reader.asks dirHome
  goPath <- Error.note InvalidGoPath maybeGoPath
  value <- execGet (CGet $ gpKey goPath)
  pure $ resolveToHomeDir homeDir $ unValue value </> fromMaybe "" (gpPath goPath)

execList :: Member (Reader Marks) r => Command 'List -> Sem r Marks
execList CList = Reader.ask
