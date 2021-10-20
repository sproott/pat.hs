import Data.Either.Extra (eitherToMaybe)
import qualified Data.Map.Strict as Map
import PatHs.Effect.Complete (Complete)
import qualified PatHs.Effect.Complete as Complete
import PatHs.Options.Complete (goPathCompleter, keyCompleter)
import PatHs.Prelude hiding (Predicate, just, right)
import PatHs.Types
import PatHs.Types.Env
import Polysemy (Sem, interpret, run)
import qualified Polysemy.Error as Error
import qualified Polysemy.Reader as Reader
import Test.Predicates
  ( Predicate (accept, explain),
    eq,
    isEmpty,
    right,
    unorderedElemsAre,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertFailure,
    testCase,
  )

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testGroup "Tests" [testComplete]

testComplete :: TestTree
testComplete =
  testGroup
    "PatHs.Options.Complete"
    [ testKeyCompleter,
      testGoPathCompleter
    ]

testKeyCompleter :: TestTree
testKeyCompleter =
  testGroup
    "keyCompleter"
    [ testCase "No match" $
        withMarks [("bar", "baz")] $ \marks ->
          assert
            (keyCompleter' "f" marks)
            isEmpty,
      testCase "Single exact match" $
        withMarks [("foo", "bar"), ("bar", "baz")] $ \marks ->
          assert
            (keyCompleter' "f" marks)
            $ eq ["foo"],
      testCase "Multiple matches" $
        withMarks [("foo", "bar"), ("foo2", "bar2"), ("bar", "baz")] $ \marks ->
          assert
            (keyCompleter' "f" marks)
            $ equivalent ["foo", "foo2"]
    ]
  where
    keyCompleter' :: Text -> Marks -> [Text]
    keyCompleter' str marks = keyCompleter str & Reader.runReader dirs & Reader.runReader marks & run

testGoPathCompleter :: TestTree
testGoPathCompleter =
  testGroup
    "goPathCompleter"
    [ testCase "No match" $
        withMarks [("bar", "baz")] $ \marks ->
          assert
            (goPathCompleter' "f" marks (const []))
            $ right isEmpty,
      testCase "Empty returns all marks" $
        withMarks [("foo", "bar"), ("bar", "baz")] $ \marks ->
          assert
            (goPathCompleter' "" marks (const []))
            $ right (equivalent ["foo/", "bar/"])
    ]
  where
    goPathCompleter' :: Text -> Marks -> (Text -> [Text]) -> Either AppError [Text]
    goPathCompleter' str marks complete =
      goPathCompleter str
        & Reader.runReader dirs
        & Reader.runReader marks
        & runCompletePure complete
        & Error.runError
        & run

equivalent :: (Eq a, Show a) => [a] -> Predicate [a]
equivalent = unorderedElemsAre . fmap eq

assert :: a -> Predicate a -> Assertion
assert x p = if accept p x then pure () else assertFailure $ explain p x

dirs :: Dirs
dirs = Dirs {dirConfig = "/home/user/.local/share/paths", dirCurrent = "/home/user", dirHome = homeDir}

homeDir :: HomeDir
homeDir = HomeDir "/home/user"

withMarks :: [(Text, Text)] -> (Marks -> Assertion) -> Assertion
withMarks list test = do
  let marks = mkMarks list
  case marks of
    Just marks -> test marks
    Nothing -> assertFailure "Failed to verify marks"

mkMarks :: [(Text, Text)] -> Maybe Marks
mkMarks = eitherToMaybe . fmap Map.fromList . traverse (bitraverse (validateKey . Key) (pure . unResolveToHomeDir homeDir))

runCompletePure :: (Text -> [Text]) -> Sem (Complete ': r) a -> Sem r a
runCompletePure complete = interpret $ \case
  Complete.CompleteDirectory str -> pure $ complete str
