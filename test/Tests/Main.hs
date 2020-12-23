module Main where

import Test.Tasty
import Tests.Blocks
import Tests.Inlines
import Tests.Metadata

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [functionalTests]

functionalTests :: TestTree
functionalTests =
  testGroup
    "functional tests"
    [ blockUnitTests,
      inlineUnitTests,
      metadataUnitTests
    ]
