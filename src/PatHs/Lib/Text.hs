module PatHs.Lib.Text where

import qualified Data.Text as Text
import Relude

replacePrefix :: Text -> Text -> Text -> Text
replacePrefix search replace text =
  case Text.stripPrefix search text of
    Just suffix -> replace <> suffix
    _ -> text
