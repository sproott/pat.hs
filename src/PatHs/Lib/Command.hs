{-# LANGUAGE KindSignatures #-}

module PatHs.Lib.Command where

import qualified Data.Map.Strict as Map
import Effectful
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as Reader
import PatHs.Effect.Error (Error)
import qualified PatHs.Effect.Error as Error
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import System.FilePath.Text ((</>))
import qualified Data.Text as T

type ExecCommand (c :: CommandType) a es = '[Error AppError, Reader Marks] ::> es => Command c -> Eff es a

validateKey' :: Error AppError :> es => Key -> Eff es ValidKey
validateKey' = Error.fromEither . validateKey

execSave :: Member (Reader Dirs) r => ExecCommand Save Marks r
execSave (CSave key forceOverwrite) = do
  marks <- Reader.ask
  validKey <- validateKey' key
  case (Map.lookup validKey marks, forceOverwrite) of
    (Just val, False) -> Error.throw $ AlreadyExists key val
    _ -> do
      homeDir <- Reader.asks dirHome
      value <- unResolveToHomeDir homeDir . T.pack <$> Reader.asks dirCurrent
      pure $ Map.insert validKey value marks

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
    Nothing -> Error.throwError $ NotExists key
    Just value -> pure value

execGo :: Member (Reader Dirs) r => ExecCommand Go ResolvedValue r
execGo (CGo maybeGoPath) = do
  homeDir <- Reader.asks dirHome
  goPath <- Error.note InvalidGoPath maybeGoPath
  value <- execGet (CGet $ gpKey goPath)
  pure $ resolveToHomeDir homeDir $ unValue value </> fromMaybe "" (gpPath goPath)

execList :: Reader Marks :> es => Command List -> Eff es Marks
execList CList = Reader.ask
