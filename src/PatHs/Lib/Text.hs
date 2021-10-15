module PatHs.Lib.Text where

import qualified Data.Text as T
import PatHs.Prelude

replacePrefix :: Text -> Text -> Text -> Text
replacePrefix search replace text =
  case T.stripPrefix search text of
    Just suffix -> replace <> suffix
    _ -> text
