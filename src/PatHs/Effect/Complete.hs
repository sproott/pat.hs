{-# LANGUAGE TemplateHaskell #-}

module PatHs.Effect.Complete where

import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import Options.Applicative (bashCompleter)
import Options.Applicative.Types (runCompleter)
import PatHs.Prelude

data Complete :: Effect where
  CompleteDirectory :: Text -> Complete m [Text]

type instance DispatchOf Complete = 'Dynamic

completeDirectory :: Complete :> es => Text -> Eff es [Text]
completeDirectory = send . CompleteDirectory

runCompleteIO :: IOE :> es => Eff (Complete ': es) a -> Eff es a
runCompleteIO = interpret $ \_ -> \case
  CompleteDirectory directory -> liftIO $ fmap (fmap T.pack) $ runCompleter (bashCompleter "directory") $ T.unpack directory
