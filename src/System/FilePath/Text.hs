module System.FilePath.Text
  ( (</>),
    addTrailingPathSeparator,
    dropTrailingPathSeparator,
    hasTrailingPathSeparator,
    makeRelative,
  )
where

import qualified Data.Text as Text
import Relude
import qualified System.FilePath as FP

convert :: (FilePath -> FilePath) -> Text -> Text
convert f = Text.pack . f . Text.unpack

convert2 :: (FilePath -> FilePath -> FilePath) -> Text -> Text -> Text
convert2 f = (Text.pack .) . (f `on` Text.unpack)

(</>) :: Text -> Text -> Text
(</>) = convert2 (FP.</>)

addTrailingPathSeparator :: Text -> Text
addTrailingPathSeparator = convert FP.addTrailingPathSeparator

dropTrailingPathSeparator :: Text -> Text
dropTrailingPathSeparator = convert FP.dropTrailingPathSeparator

hasTrailingPathSeparator :: Text -> Bool
hasTrailingPathSeparator = FP.hasTrailingPathSeparator . Text.unpack

makeRelative :: Text -> Text -> Text
makeRelative = convert2 FP.makeRelative
