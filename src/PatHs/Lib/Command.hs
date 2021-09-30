{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module PatHs.Lib.Command where

import           Data.Either.Extra    (maybeToEither)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           PatHs.Types
import           System.FilePath.Text (dropTrailingPathSeparator, (</>))

type ExecCommand (c :: CommandType) = Command c -> Marks -> Either Error (ReturnType c)

save :: ExecCommand Save
save (CSave key value) marks = do
  validKey <- validateKey key
  case Map.lookup validKey marks of
    Just value -> Left $ AlreadyExists key value
    Nothing    -> pure $ RTSave $ Map.insert validKey value marks

delete :: ExecCommand Delete
delete (CDelete key) marks = do
  get' (CGet key) marks
  validKey <- validateKey key
  pure $ RTDelete $ Map.delete validKey marks

get :: ExecCommand Get
get command marks = RTGet <$> get' command marks

get' :: Command Get -> Marks -> Either Error Value
get' (CGet key) marks = do
  validKey <- validateKey key
  case Map.lookup validKey marks of
    Nothing    -> Left $ NotExists key
    Just value -> pure value

go :: ExecCommand Go
go (CGo homeDir goPath) marks = do
  goPath <- maybeToEither InvalidGoPath goPath
  value <- get' (CGet $ Key (dropTrailingPathSeparator $ unKey $ key goPath)) marks
  pure $ RTGo $ resolveToHomeDir homeDir $ unValue value </> fromMaybe "" (path goPath)

list :: ExecCommand List
list CList = pure . RTList

runCommand :: ExecCommand c
runCommand command@CSave {}   = save command
runCommand command@CDelete {} = delete command
runCommand command@CGet {}    = get command
runCommand command@CGo {}     = go command
runCommand command@CList      = list command
