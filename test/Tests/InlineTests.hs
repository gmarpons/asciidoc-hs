module Main where

import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.AsciiDoc.Inlines
import qualified Text.Parsec as Parsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Inlines: Unit tests"
    [ testCase "Single-line no formatting marks" $
        Parsec.parse pInlines "" "some words with no format"
          @?= Right (Word "some" :| [Space, Word "words", Space, Word "with", Space, Word "no", Space, Word "format"]),
      testCase "Space at the beginning" $ assertBool "Parser doesn't fail"
        $ isLeft
        $ Parsec.parse pInlines "" " some words preceded by space",
      testCase "No formatting marks with space at the end" $
        Parsec.parse pInlines "" "some words with no format "
          @?= Right (Word "some" :| [Space, Word "words", Space, Word "with", Space, Word "no", Space, Word "format", Space]),
      testCase "Single-line strong string" $
        Parsec.parse pInlines "" "*a sentence all in strong*"
          @?= Right (Strong [Word "a", Space, Word "sentence", Space, Word "all", Space, Word "in", Space, Word "strong"] :| []),
      testCase "Single-line strong string with space at the end" $
        Parsec.parse pInlines "" "*a sentence all in strong* "
          @?= Right (Strong [Word "a", Space, Word "sentence", Space, Word "all", Space, Word "in", Space, Word "strong"] :| [Space]),
      testCase "A word in strong in the middle" $
        Parsec.parse pInlines "" "a *few* words"
          @?= Right (Word "a" :| [Space, Strong [Word "few"], Space, Word "words"]),
      testCase "Two words in strong at the beginning" $
        Parsec.parse pInlines "" "*a few* words"
          @?= Right (Strong [Word "a", Space, Word "few"] :| [Space, Word "words"]),
      testCase "Two words in strong at the end" $
        Parsec.parse pInlines "" "a *few words*"
          @?= Right (Word "a" :| [Space, Strong [Word "few", Space, Word "words"]]),
      testCase "Bad strong ending with closing mark after space and before word" $
        Parsec.parse pInlines "" "*a *few words"
          @?= Right (Symbol "*" :| [Word "a", Space, Symbol "*", Word "few", Space, Word "words"]),
      testCase "Asterisk in the middle of strong phrase" $
        Parsec.parse pInlines "" "*a *few words*"
          @?= Right (Strong [Word "a", Space, Symbol "*", Word "few", Space, Word "words"] :| []),
      testCase "Single asterisk in phrase" $
        Parsec.parse pInlines "" "a *few words"
          @?= Right (Word "a" :| [Space, Symbol "*", Word "few", Space, Word "words"]),
      testCase "Single asterisk in phrase with space at the end" $
        Parsec.parse pInlines "" "a *few words "
          @?= Right (Word "a" :| [Space, Symbol "*", Word "few", Space, Word "words", Space]),
      testCase "Single asterisk in the middle of a word" $
        Parsec.parse pInlines "" "a f*ew words"
          @?= Right (Word "a" :| [Space, Word "f", Symbol "*", Word "ew", Space, Word "words"]),
      testCase "An asterisk in the middle of a word in strong phrase" $
        Parsec.parse pInlines "" "*a f*ew* words"
          @?= Right (Strong [Word "a", Space, Word "f", Symbol "*", Word "ew"] :| [Space, Word "words"])
    ]
