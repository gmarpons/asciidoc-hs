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
          @?= Right (Str "some" :| [Space, Str "words", Space, Str "with", Space, Str "no", Space, Str "format"]),
      testCase "Space at the beginning" $ assertBool "Parser doesn't fail"
        $ isLeft
        $ Parsec.parse pInlines "" " some words preceded by space",
      testCase "No formatting marks with space at the end" $
        Parsec.parse pInlines "" "some words with no format "
          @?= Right (Str "some" :| [Space, Str "words", Space, Str "with", Space, Str "no", Space, Str "format", Space]),
      testCase "Single-line strong string" $
        Parsec.parse pInlines "" "*a sentence all in strong*"
          @?= Right (Strong [Str "a", Space, Str "sentence", Space, Str "all", Space, Str "in", Space, Str "strong"] :| []),
      testCase "Single-line strong string with space at the end" $
        Parsec.parse pInlines "" "*a sentence all in strong* "
          @?= Right (Strong [Str "a", Space, Str "sentence", Space, Str "all", Space, Str "in", Space, Str "strong"] :| [Space]),
      testCase "A word in strong in the middle" $
        Parsec.parse pInlines "" "a *few* words"
          @?= Right (Str "a" :| [Space, Strong [Str "few"], Space, Str "words"]),
      testCase "Two words in strong at the beginning" $
        Parsec.parse pInlines "" "*a few* words"
          @?= Right (Strong [Str "a", Space, Str "few"] :| [Space, Str "words"]),
      testCase "Two words in strong at the end" $
        Parsec.parse pInlines "" "a *few words*"
          @?= Right (Str "a" :| [Space, Strong [Str "few", Space, Str "words"]]),
      testCase "Bad strong ending with closing mark after space and before word" $
        Parsec.parse pInlines "" "*a *few words"
          @?= Right (Symbol "*" :| [Str "a", Space, Symbol "*", Str "few", Space, Str "words"]),
      testCase "Asterisk in the middle of strong phrase" $
        Parsec.parse pInlines "" "*a *few words*"
          @?= Right (Strong [Str "a", Space, Symbol "*", Str "few", Space, Str "words"] :| []),
      testCase "Single asterisk in phrase" $
        Parsec.parse pInlines "" "a *few words"
          @?= Right (Str "a" :| [Space, Symbol "*", Str "few", Space, Str "words"]),
      testCase "Single asterisk in phrase with space at the end" $
        Parsec.parse pInlines "" "a *few words "
          @?= Right (Str "a" :| [Space, Symbol "*", Str "few", Space, Str "words", Space]),
      testCase "Single asterisk in the middle of a word" $
        Parsec.parse pInlines "" "a f*ew words"
          @?= Right (Str "a" :| [Space, Str "f", Symbol "*", Str "ew", Space, Str "words"]),
      testCase "An asterisk in the middle of a word in strong phrase" $
        Parsec.parse pInlines "" "*a f*ew* words"
          @?= Right (Strong [Str "a", Space, Str "f", Symbol "*", Str "ew"] :| [Space, Str "words"])
    ]
