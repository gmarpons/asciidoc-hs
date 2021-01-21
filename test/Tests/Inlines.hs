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
    "Inline unit tests"
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
    [ testCase "Single-line, no formatting marks" $ do
        i <- parseInline "some words with no format"
        i `shouldBe` InlineSeq (Word "some" :| [Space " ", Word "words", Space " ", Word "with", Space " ", Word "no", Space " ", Word "format"]),
      -- testCase "Space at the beginning" $ assertBool "Parser doesn't fail"
      --   $ isLeft
      --   $ parseTest pInlines " some words preceded by space",
      testCase "No formatting marks with space at the end" $ do
        i <- parseInline "some words with no format "
        i `shouldBe` InlineSeq (Word "some" :| [Space " ", Word "words", Space " ", Word "with", Space " ", Word "no", Space " ", Word "format", Space " "])
    ]

boldInlineTests :: TestTree
boldInlineTests =
  testGroup
    "Bold inlines"
    [ testCase "Single-line, bold string" $ do
        i <- parseInline "*a sentence all in strong*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "sentence", Space " ", Word "all", Space " ", Word "in", Space " ", Word "strong"]) "*" :| []),
      testCase "Single-line, bold string with space at the end" $ do
        i <- parseInline "*a sentence all in strong* "
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "sentence", Space " ", Word "all", Space " ", Word "in", Space " ", Word "strong"]) "*" :| [Space " "]),
      testCase "A word in bold in the middle" $ do
        i <- parseInline "a *few* words"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", StyledText Bold defaultParameterList "*" (Word "few" :| []) "*", Space " ", Word "words"]),
      testCase "Two words in bold at the beginning" $ do
        i <- parseInline "*a few* words"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "few"]) "*" :| [Space " ", Word "words"]),
      testCase "Two words in bold at the end" $ do
        i <- parseInline "a *few words*"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", StyledText Bold defaultParameterList "*" (Word "few" :| [Space " ", Word "words"]) "*"]),
      testCase "Bad bold ending with closing mark after space and before word" $ do
        i <- parseInline "*a *few words"
        i `shouldBe` InlineSeq (Symbol "*" :| [Word "a", Space " ", Symbol "*", Word "few", Space " ", Word "words"]),
      testCase "Asterisk in the middle of bold phrase" $ do
        i <- parseInline "*a *few words*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words"]) "*" :| []),
      testCase "Single asterisk in phrase" $ do
        i <- parseInline "a *few words"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words"]),
      testCase "Single asterisk in phrase with space at the end" $ do
        i <- parseInline "a *few words "
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", Symbol "*", Word "few", Space " ", Word "words", Space " "]),
      testCase "Single asterisk in the middle of a word" $ do
        i <- parseInline "a f*ew words"
        i `shouldBe` InlineSeq (Word "a" :| [Space " ", Word "f", Symbol "*", Word "ew", Space " ", Word "words"]),
      testCase "An asterisk in the middle of a word in strong phrase" $ do
        i <- parseInline "*a f*ew* words"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", Word "f", Symbol "*", Word "ew"]) "*" :| [Space " ", Word "words"])
    ]

unconstrainedStylingTests :: TestTree
unconstrainedStylingTests =
  testGroup
    "Unconstrained formatting marks"
    [ testCase "Nesting unconstrained inside constrained, with no space" $ do
        i <- parseInline "#a##b##c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Word "c"]) "#" :| []),
      testCase "Unpaired opening mark before correctly closed unconstrained scope" $ do
        i <- parseInline "#a##b##"
        i `shouldBe` InlineSeq (Symbol "#" :| [Word "a", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##"]),
      testCase "Double nesting, with space (Asciidoctor does not respect nesting rule)" $ do
        i <- parseInline "## #a ##b## c# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "#", Space " ", Word "c"]) "#"]) "##" :| []),
      testCase "Unpaired opening mark directly inside unconstrained scope (libasciidoc fails test)" $ do
        i <- parseInline "## #a b ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [Symbol "#", Word "a", Space " ", Word "b", Space " "]) "##" :| []),
      testCase "Nesting constrained directly inside unconstrained, with no space" $ do
        i <- parseInline "###a###"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (StyledText Custom defaultParameterList "#" (Word "a" :| []) "#" :| []) "##" :| []),
      testCase "Unconstrained styled word" $ do
        i <- parseInline "##a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| []) "##" :| []),
      testCase "Unconstrained scope with space inside" $ do
        i <- parseInline "## a ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [Word "a", Space " "]) "##" :| []),
      testCase "Unbalanced marks, one missing on the left" $ do
        i <- parseInline "##a#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Symbol "#" :| [Word "a"]) "#" :| []),
      testCase "Unbalanced marks, one missing on the right" $ do
        i <- parseInline "#a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| []) "#" :| [Symbol "#"]),
      testCase "Nesting constrained directly inside unconstrained, with space" $ do
        i <- parseInline "## #a# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Custom defaultParameterList "#" (Word "a" :| []) "#", Space " "]) "##" :| []),
      testCase "Nesting unconstrained inside constrained, with spaces both sides" $ do
        i <- parseInline "#a ## b ## c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Space " " :| [Word "b", Space " "]) "##", Space " ", Word "c"]) "#" :| []),
      testCase "Bad nesting: constrained directly inside constrained" $ do
        i <- parseInline "#a #b# c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#", Word "b"]) "#" :| [Space " ", Word "c", Symbol "#"]),
      testCase "Two unconstrained scopes, false nesting" $ do
        i <- parseInline "##a ##b## c##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " "]) "##" :| [Word "b", StyledText Custom defaultParameterList "##" (Space " " :| [Word "c"]) "##"]),
      testCase "Unpaierd opening mark inside unconstrained scope" $ do
        i <- parseInline "##a #b ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Symbol "#", Word "b", Space " "]) "##" :| []),
      testCase "Unpaired closing mark inside unconstrained scope" $ do
        i <- parseInline "##a b# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Word "b", Symbol "#", Space " "]) "##" :| []),
      testCase "Unpaired mark between space inside unconstrained scope" $ do
        i <- parseInline "##a # ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", Symbol "#", Space " "]) "##" :| []),
      testCase "Double mark ending constrained scope" $ do
        i <- parseInline "#a ## b#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#"]) "#" :| [Space " ", Word "b", Symbol "#"]),
      testCase "Nesting constrained inside unconstrained, with spaces one side" $ do
        i <- parseInline "##a #b# c##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "#" (Word "b" :| []) "#", Space " ", Word "c"]) "##" :| []),
      testCase "Nesting unconstrained inside constrained, with spaces one side" $ do
        i <- parseInline "#a ##b## c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Space " ", Word "c"]) "#" :| []),
      testCase "Unpaired opening mark inside constrained scope" $ do
        i <- parseInline "#a ##b c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "#", Symbol "#", Word "b", Space " ", Word "c"]) "#" :| [])
    ]

nestedStylingScopeTests :: TestTree
nestedStylingScopeTests =
  testGroup
    "Mixed scopes"
    [ testCase "Constrained monospace inside italics, inside bold, no space" $ do
        i <- parseInline "*_`a`_*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (StyledText Italic defaultParameterList "_" (StyledText Monospace defaultParameterList "`" (Word "a" :| []) "`" :| []) "_" :| []) "*" :| []),
      testCase "Constrained monospace inside unconstrained italics, inside bold, no space" $ do
        i <- parseInline "*__`a`__*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (StyledText Italic defaultParameterList "__" (StyledText Monospace defaultParameterList "`" (Word "a" :| []) "`" :| []) "__" :| []) "*" :| []),
      testCase "Constrained monospace inside unconstrained italics, inside bold, some space" $ do
        i <- parseInline "*a __ `b` __c*"
        i `shouldBe` InlineSeq (StyledText Bold defaultParameterList "*" (Word "a" :| [Space " ", StyledText Italic defaultParameterList "__" (Space " " :| [StyledText Monospace defaultParameterList "`" (Word "b" :| []) "`", Space " "]) "__", Word "c"]) "*" :| []),
      testCase "Constrained italics interrupted inside custom" $ do
        i <- parseInline "#a _b#"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "#" (Word "a" :| [Space " ", Symbol "_", Word "b"]) "#" :| []),
      testCase "Constrained italics between two unconstrained custom (Asciidoctor does not respect nesting rule)" $ do
        i <- parseInline "## _a ##b## c_ ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultParameterList "##" (Space " " :| [StyledText Italic defaultParameterList "_" (Word "a" :| [Space " ", StyledText Custom defaultParameterList "##" (Word "b" :| []) "##", Space " ", Word "c"]) "_", Space " "]) "##" :| [])
    ]

scopeParameterLists :: TestTree
scopeParameterLists =
  testGroup
    "Parameter lists for scopes"
    [ testCase "Simple parameter list for constrained scope" $ do
        i <- parseInline "[underline]#a#"
        i `shouldBe` InlineSeq (StyledText Custom (ParameterList "underline") "#" (Word "a" :| []) "#" :| []),
      testCase "Simple parameter list for unconstrained scope" $ do
        i <- parseInline "[underline]##a##"
        i `shouldBe` InlineSeq (StyledText Custom (ParameterList "underline") "##" (Word "a" :| []) "##" :| []),
      testCase "Parameter list for unconstrained scope preceded by word" $ do
        i <- parseInline "a[underline]##b##"
        i `shouldBe` InlineSeq (Word "a" :| [StyledText Custom (ParameterList "underline") "##" (Word "b" :| []) "##"]),
      testCase "Parameter list for unconstrained scope succeeded by word" $ do
        i <- parseInline "[underline]##a##b"
        i `shouldBe` InlineSeq (StyledText Custom (ParameterList "underline") "##" (Word "a" :| []) "##" :| [Word "b"]),
      testCase "Failed parameter list for constrained scope preceded by word" $ do
        i <- parseInline "a[underline]#b#"
        i `shouldBe` InlineSeq (Word "a" :| [Symbol "[", Word "underline", Symbol "]", StyledText Custom (ParameterList "") "#" (Word "b" :| []) "#"]),
      testCase "Failed parameter list for constrained scope succeeded by word" $ do
        i <- parseInline "[underline]#a#b"
        i `shouldBe` InlineSeq (Symbol "[" :| [Word "underline", Symbol "]", Symbol "#", Word "a", Symbol "#", Word "b"]),
      testCase "Unfinished (failed) parameter list" $ do
        i <- parseInline "[underline"
        i `shouldBe` InlineSeq (Symbol "[" :| [Word "underline"]),
      testCase "Isolated (failed) parameter list" $ do
        i <- parseInline "[underline] #a#"
        i `shouldBe` InlineSeq (Symbol "[" :| [Word "underline", Symbol "]", Space " ", StyledText Custom (ParameterList "") "#" (Word "a" :| []) "#"])
    ]
