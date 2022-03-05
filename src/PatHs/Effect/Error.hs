module PatHs.Effect.Error (module Effectful.Error.Static, fromEither, note) where

import Effectful
import Effectful.Error.Static
import PatHs.Prelude

fromEither :: Error e :> es => Either e a -> Eff es a
fromEither (Right a) = pure a
fromEither (Left e) = throwError e

note :: Error e :> es => e -> Maybe a -> Eff es a
note _ (Just a) = pure a
note e _ = throwError e
