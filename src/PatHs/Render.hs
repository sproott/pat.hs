{-# LANGUAGE LambdaCase #-}

module PatHs.Render where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import PatHs.Types
import Prettyprinter (Doc, Pretty (pretty), annotate, dquotes, fillBreak, indent, line, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Blue, Green, Red), colorDull)
import Safe (maximumMay)

renderError :: HomeDir -> Error -> Doc AnsiStyle
renderError homeDir = annotate (colorDull Red) . convertError
  where
    convertError :: Error -> Doc a
    convertError InvalidConfig = "Invalid config"
    convertError InvalidGoPath = "Invalid go path"
    convertError ConfigNotExists = "Config does not exist"
    convertError (AlreadyExists key value) = "Key " <> dquotes (pretty $ unKey key) <> " is already mapped to value " <> dquotes (pretty $ unResolvedValue $ resolveToHomeDir homeDir $ unValue value)
    convertError (MalformedKey key) = "Malformed key " <> dquotes (pretty $ unKey key)
    convertError (NotExists key) = "Key " <> dquotes (pretty $ unKey key) <> " does not exist"

renderMarks :: ResolvedMarks -> Doc AnsiStyle
renderMarks marks = uncurry renderMarks' $ unzip $ Map.toList marks
  where
    renderMarks' :: [ValidKey] -> [ResolvedValue] -> Doc AnsiStyle
    renderMarks' keys values =
      let maxLength = fromMaybe 0 $ maximumMay $ Text.length . unValidKey <$> keys
          keysDoc = fillBreak maxLength . renderValidKey <$> keys
          valuesDoc = indent 2 . renderResolvedValue <$> values
       in vsep $ zipWith (<+>) keysDoc valuesDoc

renderValidKey :: ValidKey -> Doc AnsiStyle
renderValidKey = annotate (colorDull Blue) . pretty . unValidKey

renderResolvedValue :: ResolvedValue -> Doc AnsiStyle
renderResolvedValue = annotate (colorDull Green) . pretty . unResolvedValue

wrapWithEmptyLines :: Doc a -> Doc a
wrapWithEmptyLines doc = line <> doc <> line
