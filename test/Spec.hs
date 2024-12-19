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
import PatHs.Prelude hiding (Predicate)
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
        assert
          (keyCompleter' "c" marks)
          isEmpty,
      testCase "Single match" $
        assert
          (keyCompleter' "r" marks)
          (eq ["root"]),
      testCase "Single exact match" $
        assert
          (keyCompleter' "root" marks)
          (eq ["root"]),
      testCase "Multiple matches" $
        assert
          (keyCompleter' "h" marks)
          (equivalent ["home", "home2"]),
      testCase "Multiple matches with one exact match" $
        assert
          (keyCompleter' "home" marks)
          (equivalent ["home", "home2"])
    ]
  where
    keyCompleter' :: Text -> Marks -> [Text]
    keyCompleter' str mockMarks = keyCompleter str & Reader.runReader dirs & Reader.runReader mockMarks & runPureEff

testGoPathCompleter :: Marks -> TestTree
testGoPathCompleter marks =
  testGroup
    "goPathCompleter"
    [ testCase "No match" $
        assert
          (goPathCompleter' "config" marks (const []))
          (right isEmpty),
      testCase "Matching mark but directory does not exist" $
        assert
          (goPathCompleter' "root" marks (const []))
          (right (equivalent ["root/"])),
      testCase "Matching mark with trailing slash but directory does not exist" $
        assert
          (goPathCompleter' "root/" marks (const []))
          (right isEmpty),
      testCase "Empty returns all marks" $
        assert
          (goPathCompleter' "" marks (const []))
          (right (equivalent ["home/", "home2/", "root/", "lbin/", "macos/"])),
      testCase "Multiple matching marks" $
        assert
          (goPathCompleter' "h" marks (const []))
          (right (equivalent ["home/", "home2/"])),
      testCase "Multiple matching marks with one exact match" $
        assert
          (goPathCompleter' "home" marks (const []))
          (right (equivalent ["home/", "home2/"])),
      testCase "One matching mark completes" $
        let complete "/home/user/" = ["/home/user/.config", "/home/user/.local"]
            complete _ = []
         in assert
              (goPathCompleter' "home/" marks complete)
              (right (equivalent ["home/.config/", "home/.local/"])),
      testCase "No matching directory" $
        assert
          (goPathCompleter' "root/A" marks (const []))
          (right isEmpty),
      testCase "One matching directory cascades" $
        let complete "/home/user/" = ["/home/user/.config"]
            complete "/home/user/.config/" = ["/home/user/.config/awesome/", "/home/user/.config/nvim/"]
            complete _ = []
         in assert
              (goPathCompleter' "home/" marks complete)
              (right (equivalent ["home/.config/", "home/.config/awesome/", "home/.config/nvim/"])),
      testCase "One subdirectory does not complete immediately" $
        let complete "/root/" = ["/root/.config"]
            complete _ = []
         in assert
              (goPathCompleter' "ro" marks complete)
              (right (equivalent ["root/", "root/.config/"])),
      testCase "Multiple matching directories complete" $
        let complete "/home/user/.conf" = ["/home/user/.config/awesome", "/home/user/.config/nvim"]
            complete _ = []
         in assert
              (goPathCompleter' "home/.conf" marks complete)
              (right (equivalent ["home/.config/awesome/", "home/.config/nvim/"])),
      testCase "GoPath = \"/\" fails" $
        assert
          (goPathCompleter' "/" marks (const []))
          (left $ eq InvalidGoPath),
      testCase "GoPath starting with '/' fails" $
        assert
          (goPathCompleter' "/.config" marks (const []))
          (left $ eq InvalidGoPath)
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

equivalent :: (Eq a, Show a) => [a] -> Predicate [a]
equivalent = unorderedElemsAre . fmap eq

assert :: a -> Predicate a -> Assertion
assert x p = if accept p x then pure () else assertFailure $ explain p x

runCompletePure :: (Text -> [Text]) -> Eff (Complete : es) a -> Eff es a
runCompletePure complete = interpret $ \_ -> \case
  Complete.CompleteDirectory str -> pure $ complete str
