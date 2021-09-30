module System.FilePath.Text
  ( (</>)
  , addTrailingPathSeparator
  , makeRelative
  ) where

import           Data.Function   (on)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified System.FilePath as FP

convert :: (FilePath -> FilePath) -> Text -> Text
convert f = Text.pack . f . Text.unpack

convert2 :: (FilePath -> FilePath -> FilePath) -> Text -> Text -> Text
convert2 f = (Text.pack .) . (f `on` Text.unpack)

(</>) :: Text -> Text -> Text
(</>) = convert2 (FP.</>)

addTrailingPathSeparator :: Text -> Text
addTrailingPathSeparator = convert FP.addTrailingPathSeparator

makeRelative :: Text -> Text -> Text
makeRelative = convert2 FP.makeRelative
