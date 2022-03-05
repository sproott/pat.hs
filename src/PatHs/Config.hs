module PatHs.Config where

import Control.Arrow ((***))
import qualified Data.Map.Strict as Map
import Effectful
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as Reader
import PatHs.Parser
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env

type Config = [(Key, Value)]

configParser :: Reader Dirs :> es => Eff es (Parser Config)
configParser = do
  homeDir <- Reader.asks dirHome
  pure $ fmap (Key *** unResolveToHomeDir homeDir) <$> file

marksToConfigString :: Marks -> Text
marksToConfigString = unlines . fmap (uncurry kvToString . (unValidKey *** unValue)) . Map.toList
  where
    kvToString key value = key <> "=" <> value
