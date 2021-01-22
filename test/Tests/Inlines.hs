{-# LANGUAGE FlexibleContexts #-}

module Tests.Inlines
  ( inlineUnitTests,
  )
where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import ReprTree (reprTreeString)
import qualified Test.Hspec.Expectations.Pretty as Pretty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.AsciiDoc.Inlines
import qualified Text.Parsec as Parsec

parseInline :: Text -> IO Inline
parseInline t =
  case parseTest pInlines t of
    Right result -> pure result
    Left parseError -> assertFailure $ "Parser fails: " <> show parseError

parseTest :: Parser a -> Text -> Either Parsec.ParseError a
parseTest parser text =
  Parsec.runParser parser initialState "" text

shouldBe :: (Data a1, Data a2) => a1 -> a2 -> Pretty.Expectation
shouldBe x y = reprTreeString x `Pretty.shouldBe` reprTreeString y

inlineUnitTests :: TestTree
inlineUnitTests =
  testGroup
    "inline unit tests"
    [ simpleInlineTests,
      boldInlineTests,
      unconstrainedStylingTests,
      nestedSpanTests,
      spanParameterListTests
    ]

simpleInlineTests :: TestTree
simpleInlineTests =
  testGroup
    "simple inlines"
    [ testCase "single-line, no formatting marks" $ do
        i <- parseInline "some words with no format"
        i `shouldBe` InlineSeq (Word "some" :| [Space " ", Word "words", Space " ", Word "with", Space " ", Word "no", Space " ", Word "format"]),
      -- testCase "Space at the beginning" $ assertBool "Parser doesn't fail"
      --   $ isLeft
      --   $ parseTest pInlines " some words preceded by space",
      testCase "no formatting marks with space at the end" $ do
        i <- parseInline "some words with no format "
        i `shouldBe` InlineSeq (Word "some" :| [Space " ", Word "words", Space " ", Word "with", Space " ", Word "no", Space " ", Word "format", Space " "])
    ]

boldInlineTests :: TestTree
boldInlineTests =
  testGroup
    "bold inlines"
    [ testCase "single-line, bold string" $ do
        i <- parseInline "*a sentence all in strong*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "sentence", Space " ", Word "all", Space " ", Word "in", Space " ", Word "strong"]) "*" :| []),
      testCase "single-line, bold string with space at the end" $ do
        i <- parseInline "*a sentence all in strong* "
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "sentence", Space " ", Word "all", Space " ", Word "in", Space " ", Word "strong"]) "*" :| [Space " "]),
      testCase "a word in bold in the middle" $ do
        i <- parseInline "a *few* words"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", StyledText Bold defaultParameterList "*" (Word "few" :| []) "*", Space " ", Word "words"]),
      testCase "two words in bold at the beginning" $ do
        i <- parseInline "*a few* words"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "few"]) "*" :| [Space " ", Word "words"]),
      testCase "two words in bold at the end" $ do
        i <- parseInline "a *few words*"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", StyledText Bold defaultParameterList "*" (Word "few" :| [Space " ", Word "words"]) "*"]),
      testCase "bad bold ending with closing mark after space and before word" $ do
        i <- parseInline "*a *few words"
        i `shouldBe` InlineSeq (Symbol "*" :| [Word "a", Space " ", Symbol "*", Word "few", Space " ", Word "words"]),
      testCase "asterisk in the middle of bold phrase" $ do
        i <- parseInline "*a *few words*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words"]) "*" :| []),
      testCase "single asterisk in phrase" $ do
        i <- parseInline "a *few words"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words"]),
      testCase "single asterisk in phrase with space at the end" $ do
        i <- parseInline "a *few words "
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words", Space " "]),
      testCase "single asterisk in the middle of a word" $ do
        i <- parseInline "a f*ew words"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", Word "f", Symbol "*", Word "ew", Space " ", Word "words"]),
      testCase "an asterisk in the middle of a word in strong phrase" $ do
        i <- parseInline "*a f*ew* words"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "f", Symbol "*", Word "ew"]) "*" :| [Space " ", Word "words"])
    ]

unconstrainedStylingTests :: TestTree
unconstrainedStylingTests =
  testGroup
    "unconstrained formatting marks"
    [ testCase "nesting unconstrained inside constrained, with no space" $ do
        i <- parseInline "#a##b##c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Word "c"]) "#" :| []),
      testCase "unpaired opening mark before correctly closed unconstrained span" $ do
        i <- parseInline "#a##b##"
        i `shouldBe` InlineSeq (Symbol "#" :| [Word "a", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##"]),
      testCase "double nesting, with space (Asciidoctor does not respect nesting rule)" $ do
        i <- parseInline "## #a ##b## c# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "#", Space " ", Word "c"]) "#"]) "##" :| []),
      testCase "unpaired opening mark directly inside unconstrained pair (libasciidoc fails test)" $ do
        i <- parseInline "## #a b ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [Symbol "#", Word "a", Space " ", Word "b", Space " "]) "##" :| []),
      testCase "nesting constrained directly inside unconstrained, with no space" $ do
        i <- parseInline "###a###"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (StyledText Custom defaultParameterList "#" (Word "a" :| []) "#" :| []) "##" :| []),
      testCase "unconstrained styled word" $ do
        i <- parseInline "##a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| []) "##" :| []),
      testCase "unconstrained formatting pair with space inside" $ do
        i <- parseInline "## a ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [Word "a", Space " "]) "##" :| []),
      testCase "unbalanced marks, one missing on the left" $ do
        i <- parseInline "##a#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Symbol "#" :| [Word "a"]) "#" :| []),
      testCase "unbalanced marks, one missing on the right" $ do
        i <- parseInline "#a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| []) "#" :| [Symbol "#"]),
      testCase "nesting constrained directly inside unconstrained, with space" $ do
        i <- parseInline "## #a# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Custom defaultParameterList "#" (Word "a" :| []) "#", Space " "]) "##" :| []),
      testCase "nesting unconstrained inside constrained, with spaces both sides" $ do
        i <- parseInline "#a ## b ## c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Space " " :| [Word "b", Space " "]) "##", Space " ", Word "c"]) "#" :| []),
      testCase "bad nesting: constrained directly inside constrained" $ do
        i <- parseInline "#a #b# c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#", Word "b"]) "#" :| [Space " ", Word "c", Symbol "#"]),
      testCase "two unconstrained pairs, false nesting" $ do
        i <- parseInline "##a ##b## c##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " "]) "##" :| [Word "b", StyledText Custom defaultParameterList "##" (Space " " :| [Word "c"]) "##"]),
      testCase "unpaierd opening mark inside unconstrained pair" $ do
        i <- parseInline "##a #b ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Symbol "#", Word "b", Space " "]) "##" :| []),
      testCase "unpaired closing mark inside unconstrained pair" $ do
        i <- parseInline "##a b# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Word "b", Symbol "#", Space " "]) "##" :| []),
      testCase "unpaired mark between space inside unconstrained pair" $ do
        i <- parseInline "##a # ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Symbol "#", Space " "]) "##" :| []),
      testCase "double mark ending constrained span" $ do
        i <- parseInline "#a ## b#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#"]) "#" :| [Space " ", Word "b", Symbol "#"]),
      testCase "nesting constrained inside unconstrained, with spaces one side" $ do
        i <- parseInline "##a #b# c##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "#" (Word "b" :| []) "#", Space " ", Word "c"]) "##" :| []),
      testCase "nesting unconstrained inside constrained, with spaces one side" $ do
        i <- parseInline "#a ##b## c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Space " ", Word "c"]) "#" :| []),
      testCase "unpaired opening mark inside constrained pair" $ do
        i <- parseInline "#a ##b c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#", Symbol "#", Word "b", Space " ", Word "c"]) "#" :| [])
    ]

nestedSpanTests :: TestTree
nestedSpanTests =
  testGroup
    "mixed formatting pairs"
    [ testCase "constrained monospace inside italics, inside bold, no space" $ do
        i <- parseInline "*_`a`_*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (StyledText Italic defaultParameterList "_" (StyledText Monospace defaultParameterList "`" (Word "a" :| []) "`" :| []) "_" :| []) "*" :| []),
      testCase "constrained monospace inside unconstrained italics, inside bold, no space" $ do
        i <- parseInline "*__`a`__*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (StyledText Italic defaultParameterList "__" (StyledText Monospace defaultParameterList "`" (Word "a" :| []) "`" :| []) "__" :| []) "*" :| []),
      testCase "constrained monospace inside unconstrained italics, inside bold, some space" $ do
        i <- parseInline "*a __ `b` __c*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", StyledText Italic defaultParameterList "__" (Space " " :| [StyledText Monospace defaultParameterList "`" (Word "b" :| []) "`", Space " "]) "__", Word "c"]) "*" :| []),
      testCase "constrained italics interrupted inside custom" $ do
        i <- parseInline "#a _b#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "_", Word "b"]) "#" :| []),
      testCase "constrained italics between two unconstrained custom (Asciidoctor does not respect nesting rule)" $ do
        i <- parseInline "## _a ##b## c_ ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Italic defaultParameterList "_" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Space " ", Word "c"]) "_", Space " "]) "##" :| [])
    ]

spanParameterListTests :: TestTree
spanParameterListTests =
  testGroup
    "parameter lists for spans"
    [ testCase "simple parameter list for constrained span" $ do
        i <- parseInline "[underline]#a#"
        i `shouldBe` InlineSeq (StyledText Custom (ParameterList "underline") "#" (Word "a" :| []) "#" :| []),
      testCase "simple parameter list for unconstrained span" $ do
        i <- parseInline "[underline]##a##"
        i `shouldBe` InlineSeq (StyledText Custom (ParameterList "underline") "##" (Word "a" :| []) "##" :| []),
      testCase "parameter list for unconstrained span preceded by word" $ do
        i <- parseInline "a[underline]##b##"
        i `shouldBe` InlineSeq (Word "a" :| [StyledText Custom (ParameterList "underline") "##" (Word "b" :| []) "##"]),
      testCase "parameter list for unconstrained span followed by word" $ do
        i <- parseInline "[underline]##a##b"
        i `shouldBe` InlineSeq (StyledText Custom (ParameterList "underline") "##" (Word "a" :| []) "##" :| [Word "b"]),
      testCase "failed parameter list for constrained span preceded by word" $ do
        i <- parseInline "a[underline]#b#"
        i `shouldBe` InlineSeq (Word "a" :| [Symbol "[", Word "underline", Symbol "]", StyledText Custom (ParameterList "") "#" (Word "b" :| []) "#"]),
      testCase "failed parameter list for constrained span followed by word" $ do
        i <- parseInline "[underline]#a#b"
        i `shouldBe` InlineSeq (Symbol "[" :| [Word "underline", Symbol "]", Symbol "#", Word "a", Symbol "#", Word "b"]),
      testCase "unfinished (failed) parameter list" $ do
        i <- parseInline "[underline"
        i `shouldBe` InlineSeq (Symbol "[" :| [Word "underline"]),
      testCase "isolated (failed) parameter list" $ do
        i <- parseInline "[underline] #a#"
        i `shouldBe` InlineSeq (Symbol "[" :| [Word "underline", Symbol "]", Space " ", StyledText Custom (ParameterList "") "#" (Word "a" :| []) "#"])
    ]
