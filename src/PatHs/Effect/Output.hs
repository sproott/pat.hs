{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.Output where

import qualified Data.Text as T
import PatHs.Prelude hiding (putStr, putStrLn)
import qualified PatHs.Prelude as IO (putStr)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)

data Output m a where
  PutStr :: Text -> Output m ()

makeSem ''Output

putAnsiDoc :: Member Output r => Doc AnsiStyle -> Sem r ()
putAnsiDoc = putStr . renderStrict . layoutPretty defaultLayoutOptions

putStrLn :: Member Output r => Text -> Sem r ()
putStrLn = putStr . (<> "\n")

runOutputIO :: Member (Embed IO) r => Sem (Output ': r) a -> Sem r a
runOutputIO = interpret $ \case
  PutStr text -> embed $ IO.putStr $ T.unpack text
