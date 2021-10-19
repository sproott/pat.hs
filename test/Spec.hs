import Data.Either.Extra (eitherToMaybe)
import qualified Data.Map.Strict as Map
import PatHs.Options.Complete (keyCompleter)
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import Polysemy (run)
import qualified Polysemy.Reader as Reader
import Test.Tasty
import Test.Tasty.HUnit

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
          keyCompleter' "f" marks @?= [],
      testCase "Single exact match" $
        withMarks [("foo", "bar"), ("bar", "baz")] $ \marks ->
          keyCompleter' "f" marks @?= ["foo"],
      testCase "Multiple matches" $
        withMarks [("foo", "bar"), ("foo2", "bar2"), ("bar", "baz")] $ \marks ->
          assertEquivalent (keyCompleter' "f" marks) ["foo", "foo2"]
    ]
  where
    keyCompleter' :: Text -> Marks -> [Text]
    keyCompleter' str marks = keyCompleter str & Reader.runReader dirs & Reader.runReader @Marks marks & run

testGoPathCompleter :: TestTree
testGoPathCompleter = testGroup "goPathCompleter" []

assertEquivalent :: (Eq a, Ord a) => [a] -> [a] -> Assertion
assertEquivalent xs ys = assertBool "Lists have same elements" $ equivalent xs ys
  where
    equivalent = (==) `on` sort

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
