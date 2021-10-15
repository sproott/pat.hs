module System.FilePath.Text
  ( (</>),
    addTrailingPathSeparator,
    dropTrailingPathSeparator,
    hasTrailingPathSeparator,
    makeRelative,
  )
where

import qualified Data.Text as T
import PatHs.Prelude
import qualified System.FilePath as FP

convert :: (FilePath -> FilePath) -> Text -> Text
convert f = T.pack . f . T.unpack

convert2 :: (FilePath -> FilePath -> FilePath) -> Text -> Text -> Text
convert2 f = (T.pack .) . (f `on` T.unpack)

(</>) :: Text -> Text -> Text
(</>) = convert2 (FP.</>)

addTrailingPathSeparator :: Text -> Text
addTrailingPathSeparator = convert FP.addTrailingPathSeparator

dropTrailingPathSeparator :: Text -> Text
dropTrailingPathSeparator = convert FP.dropTrailingPathSeparator

hasTrailingPathSeparator :: Text -> Bool
hasTrailingPathSeparator = FP.hasTrailingPathSeparator . T.unpack

makeRelative :: Text -> Text -> Text
makeRelative = convert2 FP.makeRelative
