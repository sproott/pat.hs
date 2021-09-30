module PatHs.Config (Config(..), configParser, parse, marksToConfigString, ident) where

import           Control.Arrow       ((***))
import qualified Data.Map.Strict     as Map
import           PatHs.Config.Common
import           PatHs.Types

type Config = [(Key, Value)]

configParser :: HomeDir -> Parser Config
configParser homeDir = fmap (Key *** unResolveToHomeDir homeDir) <$> file

marksToConfigString :: Marks -> String
marksToConfigString = unlines . fmap (uncurry kvToString . (unValidKey *** unValue)) . Map.toList
  where kvToString key value = key <> "=" <> value
