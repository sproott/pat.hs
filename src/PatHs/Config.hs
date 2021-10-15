module PatHs.Config where

import Control.Arrow ((***))
import qualified Data.Map.Strict as Map
import PatHs.Parser
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import Polysemy (Member, Sem)
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader

type Config = [(Key, Value)]

configParser :: Member (Reader Dirs) r => Sem r (Parser Config)
configParser = do
  homeDir <- dirHome <$> Reader.ask
  pure $ fmap (Key *** unResolveToHomeDir homeDir) <$> file

marksToConfigString :: Marks -> Text
marksToConfigString = unlines . fmap (uncurry kvToString . (unValidKey *** unValue)) . Map.toList
  where
    kvToString key value = key <> "=" <> value
