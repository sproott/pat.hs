{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module PatHs.Types
  ( Key (..),
    ValidKey (unValidKey),
    Value (unValue),
    ResolvedValue (unResolvedValue),
    GoPath (..),
    Marks,
    ResolvedMarks,
    CommandType (..),
    HomeDir (unHomeDir),
    Command (..),
    SomeCommand (..),
    ReturnType (..),
    Error (..),
    ConfigError (..),
    getHomeDirectory',
    validateKey,
    resolveToHomeDir,
    unResolveToHomeDir,
    mkGoPath,
    AppM,
    runApp,
  )
where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either.Combinators (maybeToRight)
import Data.Map.Strict (Map)
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import PatHs.Parser
import System.Directory (getHomeDirectory)
import Text.Megaparsec (MonadParsec (eof))

type AppM a = ExceptT Error IO a

runApp :: AppM a -> IO (Either Error a)
runApp = runExceptT

newtype Key = Key {unKey :: Text} deriving (Eq, Ord, Show)

newtype ValidKey = ValidKey {unValidKey :: Text} deriving (Eq, Ord, Show)

newtype Value = Value {unValue :: Text} deriving (Eq, Show)

newtype ResolvedValue = ResolvedValue {unResolvedValue :: Text} deriving (Eq, Show)

data GoPath = GoPath {key :: Key, path :: Maybe Text} deriving (Eq, Show)

type Marks = Map ValidKey Value

type ResolvedMarks = Map ValidKey ResolvedValue

data CommandType = Save | Delete | Get | List | Go

newtype HomeDir = HomeDir {unHomeDir :: Text} deriving (Eq, Show)

data Command (c :: CommandType) where
  CSave :: Key -> Value -> Command Save
  CDelete :: Key -> Command Delete
  CGet :: Key -> Command Get
  CGo :: HomeDir -> Maybe GoPath -> Command Go
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

data Error = ConfigError ConfigError | InvalidGoPath | AlreadyExists Key Value | MalformedKey Key | NotExists Key deriving (Eq, Show)

data ConfigError = CERead | CEWrite | CEInvalid deriving (Eq, Show)

getHomeDirectory' :: IO HomeDir
getHomeDirectory' = HomeDir . Text.pack <$> getHomeDirectory

validateKey :: Key -> Either Error ValidKey
validateKey key@(Key str) = ValidKey <$> parse (MalformedKey key) (ident <* eof) str

homeDirVariable :: Text
homeDirVariable = "$HOME"

resolveToHomeDir :: HomeDir -> Text -> ResolvedValue
resolveToHomeDir (HomeDir homeDir) path =
  ResolvedValue $
    if homeDirVariable `isPrefixOf` path
      then homeDir <> Text.drop (Text.length homeDirVariable) path
      else path

unResolveToHomeDir :: HomeDir -> Text -> Value
unResolveToHomeDir (HomeDir homeDir) path =
  if homeDir `isPrefixOf` path
    then Value $ homeDirVariable <> Text.drop (Text.length homeDir) path
    else Value path

mkGoPath :: Text -> Either Error GoPath
mkGoPath param = do
  (keyStr, goPathStr) <- parse InvalidGoPath splitGoPath param
  keyStr <- maybeToRight InvalidGoPath keyStr
  pure $ GoPath (Key keyStr) goPathStr
