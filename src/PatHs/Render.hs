module PatHs.Render where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import PatHs.Prelude
import PatHs.Types
import Prettyprinter (Doc, Pretty (pretty), annotate, dquotes, fillBreak, indent, line, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Blue, Green, Red), colorDull)

renderError :: HomeDir -> AppError -> Doc AnsiStyle
renderError homeDir = annotate (colorDull Red) . convertError
  where
    convertError :: AppError -> Doc a
    convertError (ConfigError CEInvalid) = "Invalid config"
    convertError (ConfigError CEWrite) = "Error writing config file"
    convertError (ConfigError CERead) = "Error reading config file"
    convertError InvalidGoPath = "Invalid go path"
    convertError (AlreadyExists key value) = let 
      key' = pretty $ unKey key
      value' = pretty $ unResolvedValue $ resolveToHomeDir homeDir $ unValue value
      in mconcat ["Key ", dquotes key', " is already mapped to the following path: ", line, value', line, line, "Run with -f or --force flag to overwrite."]
    convertError (MalformedKey key) = "Malformed key " <> dquotes (pretty $ unKey key)
    convertError (NotExists key) = "Key " <> dquotes (pretty $ unKey key) <> " does not exist"

renderMarks :: ResolvedMarks -> Doc AnsiStyle
renderMarks marks = uncurry renderMarks' $ unzip $ Map.toList marks
  where
    renderMarks' :: [ValidKey] -> [ResolvedValue] -> Doc AnsiStyle
    renderMarks' keys values =
      let maxLength = fromMaybe 0 $ maximumMay $ T.length . unValidKey <$> keys
          keysDoc = fillBreak maxLength . renderValidKey <$> keys
          valuesDoc = indent 2 . renderResolvedValue <$> values
       in vsep $ zipWith (<+>) keysDoc valuesDoc

renderValidKey :: ValidKey -> Doc AnsiStyle
renderValidKey = annotate (colorDull Blue) . pretty . unValidKey

renderResolvedValue :: ResolvedValue -> Doc AnsiStyle
renderResolvedValue = annotate (colorDull Green) . pretty . unResolvedValue

wrapWithEmptyLines :: Doc a -> Doc a
wrapWithEmptyLines doc = line <> doc <> line
