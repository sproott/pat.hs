{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module PatHs.Types (
  Key(..),
  ValidKey(unValidKey),
  Value(..),
  Marks,
  CommandType(..),
  Command(..),
  SomeCommand(..),
  ReturnType(..),
  Error(..),
  validateKey,
  marks
) where

import           Control.Arrow   ((***))
import qualified Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Key = Key String deriving (Eq, Ord, Show)
newtype ValidKey = ValidKey {unValidKey :: String} deriving (Eq, Ord, Show)
newtype Value = Value String deriving (Eq, Show)

type Marks = Map ValidKey Value

data CommandType = Save | Delete | Get | List

data Command (c :: CommandType) where
  CSave :: Key -> Value -> Command Save
  CDelete :: Key -> Command Delete
  CGet :: Key -> Command Get
  CList :: Command List

deriving instance Eq (Command c)
deriving instance Show (Command c)

data SomeCommand = forall (c :: CommandType). SomeCommand (Command c)

data ReturnType (c :: CommandType) where
  RTSave :: Marks -> ReturnType Save
  RTDelete :: Marks -> ReturnType Delete
  RTGet :: Value -> ReturnType Get
  RTList :: Marks -> ReturnType List

deriving instance Eq (ReturnType c)
deriving instance Show (ReturnType c)

data Error = InvalidConfig | ConfigNotExists | AlreadyExists Key Value | MalformedKey Key | NotExists Key deriving (Eq, Show)

validateKey :: Key -> Either Error ValidKey
validateKey key@(Key str) = if '/' `elem` str
  then Left $ MalformedKey key
  else pure $ ValidKey str

marks :: Marks
marks = Map.fromList $ (ValidKey *** Value) <$> [("programming", "/home/davidh/Data/Programming"), ("nvim", "/home/davidh/.config/nvim")]
