{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.Complete where

import qualified Data.Text as T
import Options.Applicative (bashCompleter)
import Options.Applicative.Types (runCompleter)
import PatHs.Prelude
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)

data Complete m a where
  CompleteDirectory :: Text -> Complete m [Text]

makeSem ''Complete

runCompleteIO :: Member (Embed IO) r => Sem (Complete : r) a -> Sem r a
runCompleteIO = interpret $ \case
  CompleteDirectory directory -> embed $ fmap (fmap T.pack) $ runCompleter (bashCompleter "directory") $ T.unpack directory
