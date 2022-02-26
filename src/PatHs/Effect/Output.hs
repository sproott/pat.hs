{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.Output (putAnsiDoc, putStrLn, runOutputIO, module Output) where

import qualified Data.Text as T
import PatHs.Prelude hiding (putStr, putStrLn)
import qualified PatHs.Prelude as IO (putStr)
import Polysemy (Embed, Member, Sem)
import Polysemy.Output as Output
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)

putAnsiDoc :: Member (Output Text) r => Doc AnsiStyle -> Sem r ()
putAnsiDoc = Output.output . renderStrict . layoutPretty defaultLayoutOptions

putStrLn :: Member (Output Text) r => Text -> Sem r ()
putStrLn = Output.output . (<> "\n")

runOutputIO :: Member (Embed IO) r => Sem (Output Text : r) a -> Sem r a
runOutputIO = runOutputSem (IO.putStr . T.unpack)
