module PatHs.Effect.Error (module Effectful.Error.Static, fromEither, note, runError_) where

import Control.Arrow
import Effectful
import Effectful.Error.Static
import PatHs.Prelude

fromEither :: Error e :> es => Either e a -> Eff es a
fromEither (Right a) = pure a
fromEither (Left e) = throwError e

note :: Error e :> es => e -> Maybe a -> Eff es a
note _ (Just a) = pure a
note e _ = throwError e

runError_ :: Eff (Error e ': es) a -> Eff es (Either e a)
runError_ = fmap (left snd) . runError
