import PatHs.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite =
  testCase "42 is 42" $ 42 @?= 42
