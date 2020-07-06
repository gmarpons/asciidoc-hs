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
      testCase "No formatting marks with space at the end" $
        Parsec.parse pInlines "" "some words with no format "
          @?= Right (Word "some" :| [Space, Word "words", Space, Word "with", Space, Word "no", Space, Word "format", Space]),
      testCase "Single-line bold string" $
        Parsec.parse pInlines "" "*a sentence all in bold*"
          @?= Right (Bold (Word "a" :| [Space, Word "sentence", Space, Word "all", Space, Word "in", Space, Word "bold"]) :| []),
      testCase "Single-line bold string with space at the end" $
        Parsec.parse pInlines "" "*a sentence all in bold* "
          @?= Right (Bold (Word "a" :| [Space, Word "sentence", Space, Word "all", Space, Word "in", Space, Word "bold"]) :| [Space]),
      testCase "A word in bold in the middle" $
        Parsec.parse pInlines "" "a *few* words"
          @?= Right (Word "a" :| [Space, Bold (Word "few" :|[]), Space, Word "words"]),
      testCase "Two words in bold at the beginning" $
        Parsec.parse pInlines "" "*a few* words"
          @?= Right (Bold (Word "a" :| [Space, Word "few"]) :| [Space, Word "words"]),
      testCase "Two words in bold at the end" $
        Parsec.parse pInlines "" "a *few words*"
          @?= Right (Word "a" :| [Space, Bold (Word "few" :| [Space, Word "words"])]),
      testCase "Bad bold ending with closing mark after space and before word" $
        Parsec.parse pInlines "" "*a *few words"
          @?= Right (Fallback "*" :| [Word "a", Space, Fallback "*", Word "few", Space, Word "words"]),
      testCase "Asterisk in the middle of bold phrase" $
        Parsec.parse pInlines "" "*a *few words*"
          @?= Right (Bold (Word "a" :| [Space, Fallback "*", Word "few", Space, Word "words"]) :| []),
      testCase "Single asterisk in phrase" $
        Parsec.parse pInlines "" "a *few words"
          @?= Right (Word "a" :| [Space, Fallback "*", Word "few", Space, Word "words"]),
      testCase "Single asterisk in phrase with space at the end" $
        Parsec.parse pInlines "" "a *few words "
          @?= Right (Word "a" :| [Space, Fallback "*", Word "few", Space, Word "words", Space]),
      testCase "Single asterisk in the middle of a word" $
        Parsec.parse pInlines "" "a f*ew words"
          @?= Right (Word "a" :| [Space, Word "f", Fallback "*", Word "ew", Space, Word "words"]),
      testCase "An asterisk in the middle of a word in bold phrase" $
        Parsec.parse pInlines "" "*a f*ew* words"
          @?= Right (Bold (Word "a" :| [Space, Word "f", Fallback "*", Word "ew"]) :| [Space, Word "words"])
    ]
