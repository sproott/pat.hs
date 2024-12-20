{-# LANGUAGE KindSignatures #-}

module PatHs.Lib.Command where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Effectful
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as Reader
import PatHs.Effect.Error (Error)
import qualified PatHs.Effect.Error as Error
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import System.FilePath.Text ((</>))

type ExecCommand (c :: CommandType) a es = (Error AppError :> es, Reader Marks :> es) => Command c -> Eff es a

validateKey' :: Error AppError :> es => Key -> Eff es ValidKey
validateKey' = Error.fromEither . validateKey

execSave :: Reader Dirs :> es => ExecCommand Save Marks es
execSave (CSave key forceOverwrite) = do
  marks <- Reader.ask
  validKey <- validateKey' key
  case (Map.lookup validKey marks, forceOverwrite) of
    (Just val, False) -> Error.throwError $ AlreadyExists key val
    _ -> do
      homeDir <- Reader.asks dirHome
      value <- unResolveToHomeDir homeDir . T.pack <$> Reader.asks dirCurrent
      pure $ Map.insert validKey value marks

execRemove :: ExecCommand Remove Marks es
execRemove (CRemove keys) = do
  marks <- Reader.ask
  validKeys <- forM keys $ \key -> do
    _ <- execGet (CGet key)
    validateKey' key
  pure $ foldr Map.delete marks validKeys

execRename :: ExecCommand Rename Marks es
execRename (CRename key newKey) = do
  marks <- Reader.ask
  value <- execGet (CGet key)
  validKey <- validateKey' key
  validNewKey <- validateKey' newKey
  pure $ Map.insert validNewKey value $ Map.delete validKey marks

execGet :: ExecCommand Get Value es
execGet (CGet key) = do
  marks <- Reader.ask
  validKey <- validateKey' key
  case Map.lookup validKey marks of
    Nothing -> Error.throwError $ NotExists key
    Just value -> pure value

execGo :: Reader Dirs :> r => ExecCommand Go ResolvedValue r
execGo (CGo maybeGoPath) = do
  homeDir <- Reader.asks dirHome
  goPath <- Error.note InvalidGoPath maybeGoPath
  value <- execGet (CGet $ gpKey goPath)
  pure $ resolveToHomeDir homeDir $ unValue value </> fromMaybe "" (gpPath goPath)

execList :: Reader Marks :> es => Command List -> Eff es Marks
execList CList = Reader.ask
