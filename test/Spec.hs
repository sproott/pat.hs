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
import PatHs.Prelude hiding (Predicate, just, left, right)
import PatHs.Types
import PatHs.Types.Env
import Test.Predicates
  ( Predicate (accept, explain),
    eq,
    isEmpty,
    left,
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
    [ testCase "No match" $ do
        marks <- verifyMarks marks
        assert
          (keyCompleter' "c" marks)
          isEmpty,
      testCase "Single match" $ do
        marks <- verifyMarks marks
        assert
          (keyCompleter' "r" marks)
          (eq ["root"]),
      testCase "Single exact match" $ do
        marks <- verifyMarks marks
        assert
          (keyCompleter' "root" marks)
          (eq ["root"]),
      testCase "Multiple matches" $ do
        marks <- verifyMarks marks
        assert
          (keyCompleter' "h" marks)
          (equivalent ["home", "home2"]),
      testCase "Multiple matches with one exact match" $ do
        marks <- verifyMarks marks
        assert
          (keyCompleter' "home" marks)
          (equivalent ["home", "home2"])
    ]
  where
    keyCompleter' :: Text -> Marks -> [Text]
    keyCompleter' str marks = keyCompleter str & Reader.runReader dirs & Reader.runReader marks & runPureEff

testGoPathCompleter :: TestTree
testGoPathCompleter =
  testGroup
    "goPathCompleter"
    [ testCase "No match" $ do
        marks <- verifyMarks marks
        assert
          (goPathCompleter' "config" marks (const []))
          (right isEmpty),
      testCase "Empty returns all marks" $ do
        marks <- verifyMarks marks
        assert
          (goPathCompleter' "" marks (const []))
          (right (equivalent ["home/", "home2/", "root/", "lbin/"])),
      testCase "Multiple matching marks" $ do
        marks <- verifyMarks marks
        assert
          (goPathCompleter' "h" marks (const []))
          (right (equivalent ["home/", "home2/"])),
      testCase "Multiple matching marks with one exact match" $ do
        marks <- verifyMarks marks
        assert
          (goPathCompleter' "home" marks (const []))
          (right (equivalent ["home/", "home2/"])),
      testCase "One matching mark completes" $ do
        marks <- verifyMarks marks
        let complete "/home/user" = ["/home/user/.config", "/home/user/.local"]
            complete _ = []
        assert
          (goPathCompleter' "home/" marks complete)
          (right (equivalent ["home/.config/", "home/.local/"])),
      testCase "One matching directory cascades" $ do
        marks <- verifyMarks marks
        let complete "/home/user" = ["/home/user/.config"]
            complete "/home/user/.config/" = ["/home/user/.config/awesome", "/home/user/.config/nvim"]
            complete _ = []
        assert
          (goPathCompleter' "home/" marks complete)
          (right (equivalent ["home/.config/", "home/.config/awesome/", "home/.config/nvim/"])),
      testCase "Multiple matching directories complete" $ do
        marks <- verifyMarks marks
        let complete "/home/user/.config/" = ["/home/user/.config/awesome", "/home/user/.config/nvim"]
            complete _ = []
        assert
          (goPathCompleter' "home/.config/" marks complete)
          (right (equivalent ["home/.config/awesome/", "home/.config/nvim/"])),
      testCase "GoPath = \"/\" fails" $ do
        marks <- verifyMarks marks
        assert
          (goPathCompleter' "/" marks (const []))
          (left $ eq InvalidGoPath),
      testCase "GoPath starting with '/' fails" $ do
        marks <- verifyMarks marks
        assert
          (goPathCompleter' "/.config" marks (const []))
          (left $ eq InvalidGoPath)
    ]
  where
    goPathCompleter' :: Text -> Marks -> (Text -> [Text]) -> Either AppError [Text]
    goPathCompleter' str marks complete =
      goPathCompleter str
        & Reader.runReader dirs
        & Reader.runReader marks
        & runCompletePure complete
        & Error.runErrorNoCallStack
        & runPureEff

marks :: [(Text, Text)]
marks = [("home", "/home/user"), ("home2", "/home/user2"), ("root", "/root"), ("lbin", "/home/user/.local/bin")]

dirs :: Dirs
dirs = Dirs {dirConfig = "/home/user/.local/share/paths", dirCurrent = "/home/user", dirHome = homeDir}

homeDir :: HomeDir
homeDir = HomeDir "/home/user"

verifyMarks :: [(Text, Text)] -> IO Marks
verifyMarks list = do
  let marks = mkMarks list
  case marks of
    Just marks -> pure marks
    Nothing -> assertFailure "Failed to verify marks"

mkMarks :: [(Text, Text)] -> Maybe Marks
mkMarks = eitherToMaybe . fmap Map.fromList . traverse (bitraverse (validateKey . Key) (pure . unResolveToHomeDir homeDir))

equivalent :: (Eq a, Show a) => [a] -> Predicate [a]
equivalent = unorderedElemsAre . fmap eq

assert :: a -> Predicate a -> Assertion
assert x p = if accept p x then pure () else assertFailure $ explain p x

runCompletePure :: (Text -> [Text]) -> Eff (Complete : es) a -> Eff es a
runCompletePure complete = interpret $ \_ -> \case
  Complete.CompleteDirectory str -> pure $ complete str
