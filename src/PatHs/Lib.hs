{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module PatHs.Lib where

import           Control.Applicative ((<|>))
import           Control.Arrow       ((&&&))
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           PatHs.Types

type ExecCommand (c :: CommandType) = Command c -> Marks -> Either Error (ReturnType c)

save :: ExecCommand Save
save (CSave key value) marks = do
  validKey <- validateKey key
  case Map.lookup validKey marks of
    Just value -> Left $ AlreadyExists key value
    Nothing    -> pure $ RTSave $ Map.insert validKey value marks

delete :: ExecCommand Delete
delete (CDelete key) marks = do
  get (CGet key) marks
  validKey <- validateKey key
  pure $ RTDelete $ Map.delete validKey marks

get :: ExecCommand Get
get (CGet key) marks = do
  validKey <- validateKey key
  case Map.lookup validKey marks of
    Nothing    -> Left $ NotExists key
    Just value -> pure $ RTGet value

list :: ExecCommand List
list CList = pure . RTList

runCommand :: ExecCommand c
runCommand command@(CSave _ _) = save command
runCommand command@(CDelete _) = delete command
runCommand command@(CGet _)    = get command
runCommand command@CList       = list command
