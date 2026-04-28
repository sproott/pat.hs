{-# LANGUAGE BangPatterns #-}

import Data.Either.Extra (eitherToMaybe)
import qualified Data.Map.Strict as Map
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.Reader.Static as Reader
import PatHs.Effect.Complete (Complete)
import qualified PatHs.Effect.Complete as Complete
import qualified PatHs.Effect.Error as Error
import PatHs.Options.Complete (goPathCompleter, keyCompleter)
import PatHs.Prelude
import PatHs.Types
import PatHs.Types.Env
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    (@?=),
    assertFailure,
    testCase,
  )

main :: IO ()
main = do
  !marks <- verifyMarks marksData
  defaultMain (suite marks)

suite :: Marks -> TestTree
suite marks =
  testGroup "Tests" [testComplete marks]

testComplete :: Marks -> TestTree
testComplete marks =
  testGroup
    "PatHs.Options.Complete"
    $ fmap
      ($ marks)
      [ testKeyCompleter,
        testGoPathCompleter
      ]

testKeyCompleter :: Marks -> TestTree
testKeyCompleter marks =
  testGroup
    "keyCompleter"
    [ testCase "No match" $
        assertEmpty (keyCompleter' "c" marks),
      testCase "Single match" $
        assertEqual ["root"] (keyCompleter' "r" marks),
      testCase "Single exact match" $
        assertEqual ["root"] (keyCompleter' "root" marks),
      testCase "Multiple matches" $
        assertEquivalent ["home", "home2"] (keyCompleter' "h" marks),
      testCase "Multiple matches with one exact match" $
        assertEquivalent ["home", "home2"] (keyCompleter' "home" marks)
    ]
  where
    keyCompleter' :: Text -> Marks -> [Text]
    keyCompleter' str mockMarks = keyCompleter str & Reader.runReader dirs & Reader.runReader mockMarks & runPureEff

testGoPathCompleter :: Marks -> TestTree
testGoPathCompleter marks =
  testGroup
    "goPathCompleter"
    [ testCase "No match" $
        assertRightWith assertEmpty (goPathCompleter' "config" marks (const [])),
      testCase "Matching mark but directory does not exist" $
        assertRightWith (assertEquivalent ["root/"]) (goPathCompleter' "root" marks (const [])),
      testCase "Matching mark with trailing slash but directory does not exist" $
        assertRightWith assertEmpty (goPathCompleter' "root/" marks (const [])),
      testCase "Empty returns all marks" $
        assertRightWith (assertEquivalent ["home/", "home2/", "root/", "lbin/", "macos/"]) (goPathCompleter' "" marks (const [])),
      testCase "Multiple matching marks" $
        assertRightWith (assertEquivalent ["home/", "home2/"]) (goPathCompleter' "h" marks (const [])),
      testCase "Multiple matching marks with one exact match" $
        assertRightWith (assertEquivalent ["home/", "home2/"]) (goPathCompleter' "home" marks (const [])),
      testCase "One matching mark completes" $
        let complete "/home/user/" = ["/home/user/.config", "/home/user/.local"]
            complete _ = []
         in assertRightWith (assertEquivalent ["home/.config/", "home/.local/"]) (goPathCompleter' "home/" marks complete),
      testCase "No matching directory" $
        assertRightWith assertEmpty (goPathCompleter' "root/A" marks (const [])),
      testCase "One matching directory cascades" $
        let complete "/home/user/" = ["/home/user/.config"]
            complete "/home/user/.config/" = ["/home/user/.config/awesome/", "/home/user/.config/nvim/"]
            complete _ = []
         in assertRightWith (assertEquivalent ["home/.config/", "home/.config/awesome/", "home/.config/nvim/"]) (goPathCompleter' "home/" marks complete),
      testCase "One subdirectory does not complete immediately" $
        let complete "/root/" = ["/root/.config"]
            complete _ = []
         in assertRightWith (assertEquivalent ["root/", "root/.config/"]) (goPathCompleter' "ro" marks complete),
      testCase "Multiple matching directories complete" $
        let complete "/home/user/.conf" = ["/home/user/.config/awesome", "/home/user/.config/nvim"]
            complete _ = []
         in assertRightWith (assertEquivalent ["home/.config/awesome/", "home/.config/nvim/"]) (goPathCompleter' "home/.conf" marks complete),
      testCase "GoPath = \"/\" fails" $
        assertLeft InvalidGoPath (goPathCompleter' "/" marks (const [])),
      testCase "GoPath starting with '/' fails" $
        assertLeft InvalidGoPath (goPathCompleter' "/.config" marks (const []))
    ]
  where
    goPathCompleter' :: Text -> Marks -> (Text -> [Text]) -> Either AppError [Text]
    goPathCompleter' str mockMarks complete =
      goPathCompleter str
        & Reader.runReader dirs
        & Reader.runReader mockMarks
        & runCompletePure complete
        & Error.runErrorNoCallStack
        & runPureEff

marksData :: [(Text, Text)]
marksData = [("home", "/home/user"), ("home2", "/home/user2"), ("root", "/root"), ("lbin", "/home/user/.local/bin"), ("macos", "/home/user/macOS")]

dirs :: Dirs
dirs = Dirs {dirConfig = "/home/user/.local/share/paths", dirCurrent = "/home/user", dirHome = homeDir}

homeDir :: HomeDir
homeDir = HomeDir "/home/user"

verifyMarks :: [(Text, Text)] -> IO Marks
verifyMarks list = do
  let marks = mkMarks list
  case marks of
    Just m -> pure m
    Nothing -> assertFailure "Failed to verify marks"

mkMarks :: [(Text, Text)] -> Maybe Marks
mkMarks = eitherToMaybe . fmap Map.fromList . traverse (bitraverse (validateKey . Key) (pure . unResolveToHomeDir homeDir))

assertEqual :: (Eq a, Show a) => a -> a -> Assertion
assertEqual expected actual = actual @?= expected

assertEquivalent :: (Ord a, Show a) => [a] -> [a] -> Assertion
assertEquivalent expected actual = sort actual @?= sort expected

assertEmpty :: (Eq a, Show a) => [a] -> Assertion
assertEmpty = assertEqual []

assertLeft :: (Eq e, Show e, Show a) => e -> Either e a -> Assertion
assertLeft expected = \case
  Left actual -> actual @?= expected
  Right actual -> assertFailure $ "Expected Left, got Right: " <> show actual

assertRightWith :: Show e => (a -> Assertion) -> Either e a -> Assertion
assertRightWith check = \case
  Left err -> assertFailure $ "Expected Right, got Left: " <> show err
  Right actual -> check actual

runCompletePure :: (Text -> [Text]) -> Eff (Complete : es) a -> Eff es a
runCompletePure complete = interpret $ \_ -> \case
  Complete.CompleteDirectory str -> pure $ complete str
