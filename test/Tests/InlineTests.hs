{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Tasty
import Test.Tasty.HUnit
import Text.AsciiDoc.Inlines

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [inlineUnitTests]

inlineUnitTests :: TestTree
inlineUnitTests =
  testGroup
    "Inlines"
    [ simpleInlineTests,
      boldInlineTests,
      unconstrainedStylingTests,
      nestedStylingScopeTests,
      scopeParameterLists
    ]

simpleInlineTests :: TestTree
simpleInlineTests =
  testGroup
    "Simple inlines"
    [ testCase "Single-line, no formatting markers" $
        parseTest pInlines "some words with no format"
          @?= Right (InlineSeq (Word "some" :| [Space " ", Word "words", Space " ", Word "with", Space " ", Word "no", Space " ", Word "format"])),
      -- testCase "Space at the beginning" $ assertBool "Parser doesn't fail"
      --   $ isLeft
      --   $ parseTest pInlines " some words preceded by space",
      testCase "No formatting markers with space at the end" $
        parseTest pInlines "some words with no format "
          @?= Right (InlineSeq (Word "some" :| [Space " ", Word "words", Space " ", Word "with", Space " ", Word "no", Space " ", Word "format", Space " "]))
    ]

boldInlineTests :: TestTree
boldInlineTests =
  testGroup
    "Bold inlines"
    [ testCase "Single-line, bold string" $
        parseTest pInlines "*a sentence all in strong*"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "sentence", Space " ", Word "all", Space " ", Word "in", Space " ", Word "strong"]) "*" :| [])),
      testCase "Single-line, bold string with space at the end" $
        parseTest pInlines "*a sentence all in strong* "
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "sentence", Space " ", Word "all", Space " ", Word "in", Space " ", Word "strong"]) "*" :| [Space " "])),
      testCase "A word in bold in the middle" $
        parseTest pInlines "a *few* words"
          @?= Right (InlineSeq (Word "a" :| [Space " ", StyledText Bold defaultParameterList "*" (Word "few" :| []) "*", Space " ", Word "words"])),
      testCase "Two words in bold at the beginning" $
        parseTest pInlines "*a few* words"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "few"]) "*" :| [Space " ", Word "words"])),
      testCase "Two words in bold at the end" $
        parseTest pInlines "a *few words*"
          @?= Right (InlineSeq (Word "a" :| [Space " ", StyledText Bold defaultParameterList "*" (Word "few" :| [Space " ", Word "words"]) "*"])),
      testCase "Bad bold ending with closing marker after space and before word" $
        parseTest pInlines "*a *few words"
          @?= Right (InlineSeq (Symbol "*" :| [Word "a", Space " ", Symbol "*", Word "few", Space " ", Word "words"])),
      testCase "Asterisk in the middle of bold phrase" $
        parseTest pInlines "*a *few words*"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words"]) "*" :| [])),
      testCase "Single asterisk in phrase" $
        parseTest pInlines "a *few words"
          @?= Right (InlineSeq (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words"])),
      testCase "Single asterisk in phrase with space at the end" $
        parseTest pInlines "a *few words "
          @?= Right (InlineSeq (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words", Space " "])),
      testCase "Single asterisk in the middle of a word" $
        parseTest pInlines "a f*ew words"
          @?= Right (InlineSeq (Word "a" :| [Space " ", Word "f", Symbol "*", Word "ew", Space " ", Word "words"])),
      testCase "An asterisk in the middle of a word in strong phrase" $
        parseTest pInlines "*a f*ew* words"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "f", Symbol "*", Word "ew"]) "*" :| [Space " ", Word "words"]))
    ]

unconstrainedStylingTests :: TestTree
unconstrainedStylingTests =
  testGroup
    "Unconstrained styling markers"
    [ testCase "Nesting unconstrained inside constrained, with no space" $
        parseTest pInlines "#a##b##c#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Word "c"]) "#" :| [])),
      testCase "Unpaired opening marker before correctly closed unconstrained scope" $
        parseTest pInlines "#a##b##"
          @?= Right (InlineSeq (Symbol "#" :| [Word "a", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##"])),
      testCase "Double nesting, with space (Asciidoctor does not respect nesting rule)" $
        parseTest pInlines "## #a ##b## c# ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "#", Space " ", Word "c"]) "#"]) "##" :| [])),
      testCase "Unpaired opening marker directly inside unconstrained scope (libasciidoc fails test)" $
        parseTest pInlines "## #a b ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [Symbol "#", Word "a", Space " ", Word "b", Space " "]) "##" :| [])),
      testCase "Nesting constrained directly inside unconstrained, with no space" $
        parseTest pInlines "###a###"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (StyledText Custom defaultParameterList "#" (Word "a" :| []) "#" :| []) "##" :| [])),
      testCase "Unconstrained styled word" $
        parseTest pInlines "##a##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| []) "##" :| [])),
      testCase "Unconstrained scope with space inside" $
        parseTest pInlines "## a ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [Word "a", Space " "]) "##" :| [])),
      testCase "Unbalanced markers, one missing on the left" $
        parseTest pInlines "##a#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Symbol "#" :| [Word "a"]) "#":| [])),
      testCase "Unbalanced markers, one missing on the right" $
        parseTest pInlines "#a##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| []) "#" :| [Symbol "#"])),
      testCase "Nesting constrained directly inside unconstrained, with space" $
        parseTest pInlines "## #a# ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Custom defaultParameterList "#" (Word "a" :| []) "#", Space " "]) "##" :| [])),
      testCase "Nesting unconstrained inside constrained, with spaces both sides" $
        parseTest pInlines "#a ## b ## c#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Space " " :| [Word "b", Space " "]) "##", Space " ", Word "c"]) "#" :| [])),
      testCase "Bad nesting: constrained directly inside constrained" $
        parseTest pInlines "#a #b# c#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#", Word "b"]) "#" :| [Space " ", Word "c", Symbol "#"])),
      testCase "Two unconstrained scopes, false nesting" $
        parseTest pInlines "##a ##b## c##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " "]) "##" :| [Word "b", StyledText Custom defaultParameterList "##" (Space " " :| [Word "c"]) "##"])),
      testCase "Unpaierd opening marker inside unconstrained scope" $
        parseTest pInlines "##a #b ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Symbol "#", Word "b", Space " "]) "##" :| [])),
      testCase "Unpaired closing marker inside unconstrained scope" $
        parseTest pInlines "##a b# ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Word "b", Symbol "#", Space " "]) "##" :| [])),
      testCase "Unpaired marker between space inside unconstrained scope" $
        parseTest pInlines "##a # ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Symbol "#", Space " "]) "##" :| [])),
      testCase "Double marker ending constrained scope" $
        parseTest pInlines "#a ## b#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#"]) "#" :| [Space " ", Word "b", Symbol "#"])),
      testCase "Nesting constrained inside unconstrained, with spaces one side" $
        parseTest pInlines "##a #b# c##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "#" (Word "b" :| []) "#", Space " ", Word "c"]) "##" :| [])),
      testCase "Nesting unconstrained inside constrained, with spaces one side" $
        parseTest pInlines "#a ##b## c#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Space " ", Word "c"]) "#" :| [])),
      testCase "Unpaired opening marker inside constrained scope" $
        parseTest pInlines "#a ##b c#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#", Symbol "#", Word "b", Space " ", Word "c"]) "#" :| []))
    ]

nestedStylingScopeTests :: TestTree
nestedStylingScopeTests =
  testGroup
    "Mixed scopes"
    [ testCase "Constrained monospace inside italics, inside bold, no space" $
        parseTest pInlines "*_`a`_*"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (StyledText Italic defaultParameterList "_" (StyledText Monospace defaultParameterList "`" (Word "a" :| []) "`" :| []) "_" :| []) "*" :| [])),
      testCase "Constrained monospace inside unconstrained italics, inside bold, no space" $
        parseTest pInlines "*__`a`__*"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (StyledText Italic defaultParameterList "__" (StyledText Monospace defaultParameterList "`" (Word "a" :| []) "`" :| []) "__" :| []) "*" :| [])),
      testCase "Constrained monospace inside unconstrained italics, inside bold, some space" $
        parseTest pInlines "*a __ `b` __c*"
          @?= Right (InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", StyledText Italic defaultParameterList "__" (Space " " :| [StyledText Monospace defaultParameterList "`" (Word "b" :| []) "`", Space " "]) "__", Word "c"]) "*" :| [])),
      testCase "Constrained italics interrupted inside custom" $
        parseTest pInlines "#a _b#"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "_", Word "b"]) "#" :| [])),
      testCase "Constrained italics between two unconstrained custom (Asciidoctor does not respect nesting rule)" $
        parseTest pInlines "## _a ##b## c_ ##"
          @?= Right (InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Italic defaultParameterList "_" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Space " ", Word "c"]) "_", Space " "]) "##" :| []))
    ]

scopeParameterLists :: TestTree
scopeParameterLists =
  testGroup
    "Parameter lists for scopes"
    [ testCase "Simple parameter list for constrained scope" $
        parseTest pInlines "[underline]#a#"
          @?= Right (InlineSeq (StyledText Custom (ParameterList "underline") "#" (Word "a" :| []) "#" :| [])),
      testCase "Simple parameter list for unconstrained scope" $
        parseTest pInlines "[underline]##a##"
          @?= Right (InlineSeq (StyledText Custom (ParameterList "underline") "##" (Word "a" :| []) "##" :| [])),
      testCase "Parameter list for unconstrained scope preceded by word" $
        parseTest pInlines "a[underline]##b##"
          @?= Right (InlineSeq (Word "a" :| [StyledText Custom (ParameterList "underline") "##" (Word "b" :| []) "##"])),
      testCase "Parameter list for unconstrained scope succeeded by word" $
        parseTest pInlines "[underline]##a##b"
          @?= Right (InlineSeq (StyledText Custom (ParameterList "underline") "##" (Word "a" :| []) "##" :| [Word "b"])),
      testCase "Failed parameter list for constrained scope preceded by word" $
        parseTest pInlines "a[underline]#b#"
          @?= Right (InlineSeq (Word "a" :| [Symbol "[", Word "underline", Symbol "]", StyledText Custom (ParameterList "") "#" (Word "b" :| []) "#"])),
      testCase "Failed parameter list for constrained scope succeeded by word" $
        parseTest pInlines "[underline]#a#b"
          @?= Right (InlineSeq (Symbol "[" :| [Word "underline", Symbol "]", Symbol "#", Word "a", Symbol "#", Word "b"])),
      testCase "Unfinished (failed) parameter list" $
        parseTest pInlines "[underline"
          @?= Right (InlineSeq (Symbol "[" :| [Word "underline"])),
      testCase "Isolated (failed) parameter list" $
        parseTest pInlines "[underline] #a#"
          @?= Right (InlineSeq (Symbol "[" :| [Word "underline", Symbol "]", Space " ", StyledText Custom (ParameterList "") "#" (Word "a" :| []) "#"]))
    ]
