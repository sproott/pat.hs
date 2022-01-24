module PatHs.Effect.Output (Output, output, putAnsiDoc, putStrLn, runOutputIO) where

import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import PatHs.Prelude hiding (putStr, putStrLn)
import qualified PatHs.Prelude as IO (putStr)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)

data Output o :: Effect where
  Output :: o -> Output o m ()

type instance DispatchOf (Output o) = 'Dynamic

output :: (HasCallStack, Output o :> es) => o -> Eff es ()
output = send . Output

putAnsiDoc :: Output Text :> es => Doc AnsiStyle -> Eff es ()
putAnsiDoc = output . renderStrict . layoutPretty defaultLayoutOptions

putStrLn :: Output Text :> es => Text -> Eff es ()
putStrLn = output . (<> "\n")

runOutput :: (o -> Eff es ()) -> Eff (Output o ': es) a -> Eff es a
runOutput fn = interpret $ \_ -> \case
  Output o -> fn o

runOutputIO :: IOE :> es => Eff (Output Text ': es) a -> Eff es a
runOutputIO = runOutput (IO.putStr . T.unpack)
