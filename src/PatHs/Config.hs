module PatHs.Config where

import           Control.Arrow   ((***))
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           PatHs.Parser
import           PatHs.Types

type Config = [(Key, Value)]

configParser :: HomeDir -> Parser Config
configParser homeDir = fmap (Key *** unResolveToHomeDir homeDir) <$> file

marksToConfigString :: Marks -> Text
marksToConfigString = Text.unlines . fmap (uncurry kvToString . (unValidKey *** unValue)) . Map.toList
  where
    kvToString key value = key <> "=" <> value
