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
  AppM,
  runApp
) where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Map.Strict            (Map)
import           PatHs.Config.Common
import           Text.Megaparsec            (MonadParsec (eof))

type AppM a = ExceptT Error IO a

runApp :: AppM a -> IO (Either Error a)
runApp = runExceptT

newtype Key = Key {unKey :: String} deriving (Eq, Ord, Show)
newtype ValidKey = ValidKey {unValidKey :: String} deriving (Eq, Ord, Show)
newtype Value = Value {unValue :: String} deriving (Eq, Show)

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
validateKey key@(Key str) = ValidKey <$> parse (MalformedKey key) (ident <* eof) str
