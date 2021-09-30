{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module PatHs.Types (
  Key(..),
  ValidKey(unValidKey),
  Value(unValue),
  ResolvedValue(unResolvedValue),
  GoPath(..),
  Marks,
  ResolvedMarks,
  CommandType(..),
  HomeDir(unHomeDir),
  Command(..),
  SomeCommand(..),
  ReturnType(..),
  Error(..),
  getHomeDirectory',
  validateKey,
  resolveToHomeDir,
  unResolveToHomeDir,
  AppM,
  runApp
) where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.List                  (isPrefixOf)
import           Data.Map.Strict            (Map)
import           PatHs.Config.Common
import           System.Directory           (getHomeDirectory)
import           Text.Megaparsec            (MonadParsec (eof))

type AppM a = ExceptT Error IO a

runApp :: AppM a -> IO (Either Error a)
runApp = runExceptT

newtype Key = Key {unKey :: String} deriving (Eq, Ord, Show)
newtype ValidKey = ValidKey {unValidKey :: String} deriving (Eq, Ord, Show)
newtype Value = Value {unValue :: String} deriving (Eq, Show)
newtype ResolvedValue = ResolvedValue {unResolvedValue :: String} deriving (Eq, Show)

newtype GoPath = GoPath {unGoPath :: String} deriving (Eq, Show)

type Marks = Map ValidKey Value
type ResolvedMarks = Map ValidKey ResolvedValue

data CommandType = Save | Delete | Get | List | Go

newtype HomeDir = HomeDir {unHomeDir :: FilePath} deriving (Eq, Show)

data Command (c :: CommandType) where
  CSave :: Key -> Value -> Command Save
  CDelete :: Key -> Command Delete
  CGet :: Key -> Command Get
  CGo :: HomeDir -> Key -> GoPath -> Command Go
  CList :: Command List

deriving instance Eq (Command c)
deriving instance Show (Command c)

data SomeCommand = forall (c :: CommandType). SomeCommand (Command c)

data ReturnType (c :: CommandType) where
  RTSave :: Marks -> ReturnType Save
  RTDelete :: Marks -> ReturnType Delete
  RTGet :: Value -> ReturnType Get
  RTGo :: ResolvedValue -> ReturnType Go
  RTList :: Marks -> ReturnType List

deriving instance Eq (ReturnType c)
deriving instance Show (ReturnType c)

data Error = InvalidConfig | ConfigNotExists | AlreadyExists Key Value | MalformedKey Key | NotExists Key deriving (Eq, Show)

getHomeDirectory' :: IO HomeDir
getHomeDirectory' = HomeDir <$> getHomeDirectory

validateKey :: Key -> Either Error ValidKey
validateKey key@(Key str) = ValidKey <$> parse (MalformedKey key) (ident <* eof) str

homeDirVariable :: String
homeDirVariable = "$HOME"

resolveToHomeDir :: HomeDir -> String -> ResolvedValue
resolveToHomeDir (HomeDir homeDir) path = ResolvedValue $
  if homeDirVariable `isPrefixOf` path then
    homeDir <> drop (length homeDirVariable) path
  else path

unResolveToHomeDir :: HomeDir -> String -> Value
unResolveToHomeDir (HomeDir homeDir) path = if homeDir `isPrefixOf` path
    then Value $ homeDirVariable <> drop (length homeDir) path
    else Value path
