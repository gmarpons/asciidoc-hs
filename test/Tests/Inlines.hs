module Tests.Inlines
  ( inlineUnitTests,
  )
where

import Data.Either (isLeft)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Internal (Identity (runIdentity))
import Data.Text (Text)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Text.AsciiDoc.Inlines
import qualified Text.Parsec as Parsec

parseInline :: Text -> IO Inline
parseInline t =
  case parseTest inlinesP t of
    Right result -> pure result
    Left parseError -> assertFailure $ "Parser fails: " <> show parseError

parseTest :: Parser Identity a -> Text -> Either Parsec.ParseError a
parseTest parser t =
  runIdentity $ Parsec.runParserT parser initialState "" t

inlineUnitTests :: TestTree
inlineUnitTests =
  testGroup
    "inline unit tests"
    [ simpleInlineTests,
      boldInlineTests,
      unconstrainedStylingTests,
      mixedFormattingStyleTests,
      enclosureInlineAttributeListTests,
      punctuationSymbolTests
    ]

simpleInlineTests :: TestTree
simpleInlineTests =
  testGroup
    "simple inlines"
    [ testCase "single-line, no formatting marks" $ do
        i <- parseInline "some words with no format"
        i `shouldBe` InlineSeq (AlphaNum "some" :| [Space " ", AlphaNum "words", Space " ", AlphaNum "with", Space " ", AlphaNum "no", Space " ", AlphaNum "format"]),
      testCase "Space at the beginning" $
        assertBool "Parser doesn't fail" $
          isLeft $
            parseTest inlinesP " some words preceded by space",
      testCase "no formatting marks with space at the end" $ do
        i <- parseInline "some words with no format "
        i `shouldBe` InlineSeq (AlphaNum "some" :| [Space " ", AlphaNum "words", Space " ", AlphaNum "with", Space " ", AlphaNum "no", Space " ", AlphaNum "format", Space " "])
    ]

boldInlineTests :: TestTree
boldInlineTests =
  testGroup
    "bold inlines"
    [ testCase "single-line, bold string" $ do
        i <- parseInline "*a sentence all in strong*"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", AlphaNum "sentence", Space " ", AlphaNum "all", Space " ", AlphaNum "in", Space " ", AlphaNum "strong"]) "*" :| []),
      testCase "single-line, bold string with space at the end" $ do
        i <- parseInline "*a sentence all in strong* "
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", AlphaNum "sentence", Space " ", AlphaNum "all", Space " ", AlphaNum "in", Space " ", AlphaNum "strong"]) "*" :| [Space " "]),
      testCase "a word in bold in the middle" $ do
        i <- parseInline "a *few* words"
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Space " ", StyledText Bold defaultAttributeList "*" (AlphaNum "few" :| []) "*", Space " ", AlphaNum "words"]),
      testCase "two words in bold at the beginning" $ do
        i <- parseInline "*a few* words"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", AlphaNum "few"]) "*" :| [Space " ", AlphaNum "words"]),
      testCase "two words in bold at the end" $ do
        i <- parseInline "a *few words*"
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Space " ", StyledText Bold defaultAttributeList "*" (AlphaNum "few" :| [Space " ", AlphaNum "words"]) "*"]),
      testCase "bad bold ending with closing mark after space" $ do
        i <- parseInline "*a few *"
        i `shouldBe` InlineSeq (Symbol "*" :| [AlphaNum "a", Space " ", AlphaNum "few", Space " ", Symbol "*"]),
      testCase "bad bold ending with closing mark after space and before word" $ do
        i <- parseInline "*a *few words"
        i `shouldBe` InlineSeq (Symbol "*" :| [AlphaNum "a", Space " ", Symbol "*", AlphaNum "few", Space " ", AlphaNum "words"]),
      testCase "asterisk in the middle of bold phrase" $ do
        i <- parseInline "*a *few words*"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", Symbol "*", AlphaNum "few", Space " ", AlphaNum "words"]) "*" :| []),
      testCase "single asterisk in phrase" $ do
        i <- parseInline "a *few words"
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Space " ", Symbol "*", AlphaNum "few", Space " ", AlphaNum "words"]),
      testCase "single asterisk in phrase with space at the end" $ do
        i <- parseInline "a *few words "
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Space " ", Symbol "*", AlphaNum "few", Space " ", AlphaNum "words", Space " "]),
      testCase "single asterisk in the middle of a word" $ do
        i <- parseInline "a f*ew words"
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Space " ", AlphaNum "f", Symbol "*", AlphaNum "ew", Space " ", AlphaNum "words"]),
      testCase "an asterisk in the middle of a word in strong phrase" $ do
        i <- parseInline "*a f*ew* words"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", AlphaNum "f", Symbol "*", AlphaNum "ew"]) "*" :| [Space " ", AlphaNum "words"]),
      testCase "single asterisk followed by word and space" $ do
        i <- parseInline "*a "
        i `shouldBe` InlineSeq (Symbol "*" :| [AlphaNum "a", Space " "])
    ]

unconstrainedStylingTests :: TestTree
unconstrainedStylingTests =
  testGroup
    "unconstrained formatting marks"
    [ testCase "nesting unconstrained inside constrained, with no space" $ do
        i <- parseInline "#a##b##c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [StyledText Custom defaultAttributeList "##" (AlphaNum "b" :| []) "##", AlphaNum "c"]) "#" :| []),
      testCase "unpaired opening mark before correctly closed unconstrained pair" $ do
        i <- parseInline "#a##b##"
        i `shouldBe` InlineSeq (Symbol "#" :| [AlphaNum "a", StyledText Custom defaultAttributeList "##" (AlphaNum "b" :| []) "##"]),
      --  Divergence from Asciidoctor.
      testCase "(## #a ##b## c# ##)" $ do
        i <- parseInline "## #a ##b## c# ##"
        -- let alt_result = InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", StyledText Custom defaultAttributeList "##" (AlphaNum "b" :| []) "##", Space " ", AlphaNum "c"]) "#", Space " "]) "##" :| [])
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [Symbol "#", AlphaNum "a", Space " "]) "##" :| [AlphaNum "b", StyledText Custom defaultAttributeList "##" (Space " " :| [AlphaNum "c", Symbol "#", Space " "]) "##"])
        i `shouldBe` result,
      -- libasciidoc fails test
      testCase "unpaired opening mark directly inside unconstrained pair" $ do
        i <- parseInline "## #a b ##"
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [Symbol "#", AlphaNum "a", Space " ", AlphaNum "b", Space " "]) "##" :| [])
        i `shouldBe` result,
      --  Divergence DVI001 from Asciidoctor.
      testCase "three opening marks, and three closing marks, with no space" $ do
        i <- parseInline "###a###"
        -- Asciidoctor:
        -- let alt_result = InlineSeq (StyledText Custom defaultAttributeList "##" (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| []) "#" :| []) "##" :| [])
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Symbol "#" :| [AlphaNum "a"]) "##" :| [Symbol "#"])
        i `shouldBe` result,
      -- Divergence DVI001 from Asciidoctor.
      testCase "three opening marks, and three closing marks, two words and no space" $ do
        i <- parseInline "###a b###"
        -- Asciidoctor:
        -- let alt_result = InlineSeq (StyledText Custom defaultAttributeList "##" (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", AlphaNum "b"]) "#" :| []) "##" :| [])
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Symbol "#" :| [AlphaNum "a", Space " ", AlphaNum "b"]) "##" :| [Symbol "#"])
        i `shouldBe` result,
      -- Divergence from Asciidoctor.
      -- TODO. This one is different from DVI001. Asciidoctor's outputs is just
      -- wrong. It looks like it works because it doesn't respect the nesting
      -- rule.
      testCase "three opening marks, and three closing marks, two words and no space" $ do
        -- Asciidoctor:
        -- let alt_result = InlineSeq (StyledText Custom defaultAttributeList "#" (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " ", AlphaNum "b", Space " "]) "##" :| []) "#" :| [])
        i <- parseInline "###a b ###"
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Symbol "#" :| [AlphaNum "a", Space " ", AlphaNum "b", Space " "]) "##" :| [Symbol "#"])
        i `shouldBe` result,
      testCase "unconstrained styled word" $ do
        i <- parseInline "##a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| []) "##" :| []),
      testCase "unconstrained formatting pair with inner space on the left" $ do
        i <- parseInline "## a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [AlphaNum "a"]) "##" :| []),
      testCase "unconstrained formatting pair with inner space on the right" $ do
        i <- parseInline "##a ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " "]) "##" :| []),
      testCase "unconstrained formatting pair with inner space on both sides" $ do
        i <- parseInline "## a ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [AlphaNum "a", Space " "]) "##" :| []),
      testCase "unbalanced marks, one missing on the left" $ do
        i <- parseInline "##a#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (Symbol "#" :| [AlphaNum "a"]) "#" :| []),
      testCase "unbalanced marks, one missing on the right" $ do
        i <- parseInline "#a##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| []) "#" :| [Symbol "#"]),
      testCase "nesting constrained directly inside unconstrained, with space" $ do
        i <- parseInline "## #a# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| []) "#", Space " "]) "##" :| []),
      testCase "nesting unconstrained inside constrained, with spaces everywhere" $ do
        i <- parseInline "#a ## b ## c#"
        let result = InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", StyledText Custom defaultAttributeList "##" (Space " " :| [AlphaNum "b", Space " "]) "##", Space " ", AlphaNum "c"]) "#" :| [])
        i `shouldBe` result,
      testCase "bad nesting: constrained directly inside constrained" $ do
        i <- parseInline "#a #b# c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", Symbol "#", AlphaNum "b"]) "#" :| [Space " ", AlphaNum "c", Symbol "#"]),
      testCase "two unconstrained pairs, false nesting" $ do
        i <- parseInline "##a ##b## c##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " "]) "##" :| [AlphaNum "b", StyledText Custom defaultAttributeList "##" (Space " " :| [AlphaNum "c"]) "##"]),
      testCase "unpaired opening mark inside unconstrained pair" $ do
        i <- parseInline "##a #b ##"
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " ", Symbol "#", AlphaNum "b", Space " "]) "##" :| [])
        i `shouldBe` result,
      testCase "unpaired closing mark inside unconstrained pair" $ do
        i <- parseInline "##a b# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " ", AlphaNum "b", Symbol "#", Space " "]) "##" :| []),
      testCase "unpaired mark between space inside unconstrained pair" $ do
        i <- parseInline "##a # ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " ", Symbol "#", Space " "]) "##" :| []),
      testCase "double mark ending constrained enclosure" $ do
        i <- parseInline "#a ## b#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", Symbol "#"]) "#" :| [Space " ", AlphaNum "b", Symbol "#"]),
      testCase "nesting constrained inside unconstrained, with spaces on both sides" $ do
        i <- parseInline "##a #b# c##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| [Space " ", StyledText Custom defaultAttributeList "#" (AlphaNum "b" :| []) "#", Space " ", AlphaNum "c"]) "##" :| []),
      testCase "nesting unconstrained inside constrained, with spaces on both sides" $ do
        i <- parseInline "#a ##b## c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", StyledText Custom defaultAttributeList "##" (AlphaNum "b" :| []) "##", Space " ", AlphaNum "c"]) "#" :| []),
      testCase "unpaired opening mark inside constrained pair" $ do
        i <- parseInline "#a ##b c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", Symbol "#", Symbol "#", AlphaNum "b", Space " ", AlphaNum "c"]) "#" :| []),
      testCase "unpaired opening mark after correcty closed pair and some noise" $ do
        i <- parseInline "##a##b# ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (AlphaNum "a" :| []) "##" :| [AlphaNum "b", Symbol "#", Space " ", Symbol "#", Symbol "#"]),
      testCase "three-mark sequence" $ do
        i <- parseInline "###"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (Symbol "#" :| []) "#" :| []),
      testCase "four-mark sequence" $ do
        i <- parseInline "####"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (Symbol "#" :| []) "#" :| [Symbol "#"]),
      testCase "five-mark sequence" $ do
        i <- parseInline "#####"
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Symbol "#" :| []) "##" :| [])
        i `shouldBe` result,
      -- Divergence DVI002 from Asciidoctor: Asciidoctor accepts nested
      -- enclosures with empty inline inside.
      testCase "six-mark sequence" $ do
        i <- parseInline "######"
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Symbol "#" :| []) "##" :| [Symbol "#"])
        i `shouldBe` result,
      testCase "unconstrained enclosure with a single space" $ do
        i <- parseInline "## ##"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| []) "##" :| []),
      testCase "two three-mark sequences separated by space" $ do
        i <- parseInline "### ###"
        -- Asciidoctor seems inconsistent w.r.t "###a###" because in this case
        -- it flips U and C.
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Symbol "#" :| [Space " "]) "##" :| [Symbol "#"])
        i `shouldBe` result,
      -- Divergence DVI003 from Asciidoctor: Asciidoctor doesn't parse "...#b
      -- c#" as an enclosure, but it does if the mark of both enclosures is
      -- different. We consider the behavior here more uniform.
      testCase "two constrained enclosures with the same mark an no space in between" $ do
        i <- parseInline "#a##b c#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| []) "#" :| [StyledText Custom defaultAttributeList "#" (AlphaNum "b" :| [Space " ", AlphaNum "c"]) "#"])
    ]

mixedFormattingStyleTests :: TestTree
mixedFormattingStyleTests =
  testGroup
    "mixed formatting styles"
    [ testCase "constrained monospace inside italics, inside bold, no space" $ do
        i <- parseInline "*_`a`_*"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (StyledText Italic defaultAttributeList "_" (StyledText Monospace defaultAttributeList "`" (AlphaNum "a" :| []) "`" :| []) "_" :| []) "*" :| []),
      testCase "constrained monospace inside unconstrained italics, inside bold, no space" $ do
        i <- parseInline "*__`a`__*"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (StyledText Italic defaultAttributeList "__" (StyledText Monospace defaultAttributeList "`" (AlphaNum "a" :| []) "`" :| []) "__" :| []) "*" :| []),
      testCase "constrained monospace inside unconstrained italics, inside bold, some space" $ do
        i <- parseInline "*a __ `b` __c*"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", StyledText Italic defaultAttributeList "__" (Space " " :| [StyledText Monospace defaultAttributeList "`" (AlphaNum "b" :| []) "`", Space " "]) "__", AlphaNum "c"]) "*" :| []),
      testCase "constrained italics interrupted inside custom" $ do
        i <- parseInline "#a _b#"
        i `shouldBe` InlineSeq (StyledText Custom defaultAttributeList "#" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "#" :| []),
      -- Divergence from Asciidoctor: Asciidoctor doesn't respect nesting rule.
      testCase "(## _a ##b## c_ ##)" $ do
        i <- parseInline "## _a ##b## c_ ##"
        -- let alt_result = InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [StyledText Italic defaultAttributeList "_" (AlphaNum "a" :| [Space " ", StyledText Custom defaultAttributeList "##" (AlphaNum "b" :| []) "##", Space " ", AlphaNum "c"]) "_", Space " "]) "##" :| [])
        let result = InlineSeq (StyledText Custom defaultAttributeList "##" (Space " " :| [Symbol "_", AlphaNum "a", Space " "]) "##" :| [AlphaNum "b", StyledText Custom defaultAttributeList "##" (Space " " :| [AlphaNum "c", Symbol "_", Space " "]) "##"])
        i `shouldBe` result,
      testCase "(__ #a##b##c# __)" $ do
        i <- parseInline "__ #a##b##c# __"
        let result = InlineSeq (StyledText Italic (InlineAttributeList "") "__" (Space " " :| [StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| [StyledText Custom (InlineAttributeList "") "##" (AlphaNum "b" :| []) "##", AlphaNum "c"]) "#", Space " "]) "__" :| [])
        i `shouldBe` result,
      testCase "(#a *##b##*#)" $ do
        i <- parseInline "#a *##b##*#"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| [Space " ", StyledText Bold (InlineAttributeList "") "*" (StyledText Custom (InlineAttributeList "") "##" (AlphaNum "b" :| []) "##" :| []) "*"]) "#" :| [])
        i `shouldBe` result,
      -- Divergence from Asciidoctor: Asciidoctor doesn't respect nesting rule.
      testCase "(*a _b* c_)" $ do
        i <- parseInline "*a _b* c_"
        -- let alt_result = InlineSeq (Symbol "*" :| [AlphaNum "a", Space " ", StyledText Italic defaultAttributeList "_" (AlphaNum "b" :| [Symbol "*", Space " ", AlphaNum "c"]) "_"])
        let result = InlineSeq (StyledText Bold defaultAttributeList "*" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "*" :| [Space " ", AlphaNum "c", Symbol "_"])
        i `shouldBe` result,
      -- Divergence from Asciidoctor: Asciidoctor doesn't respect nesting rule.
      testCase "(**a __b** c__)" $ do
        i <- parseInline "**a __b** c__"
        -- let alt_result = InlineSeq (Symbol "*" :| [Symbol "*", AlphaNum "a", Space " ", StyledText Italic defaultAttributeList "__" (AlphaNum "b" :| [Symbol "*", Symbol "*", Space " ", AlphaNum "c"]) "__"])
        let result = InlineSeq (StyledText Bold defaultAttributeList "**" (AlphaNum "a" :| [Space " ", Symbol "_", Symbol "_", AlphaNum "b"]) "**" :| [Space " ", AlphaNum "c", Symbol "_", Symbol "_"])
        i `shouldBe` result,
      -- Divergence from Asciidoctor: Asciidoctor doesn't respect nesting rule.
      testCase "(**a _b** c_)" $ do
        i <- parseInline "**a _b** c_"
        i `shouldBe` InlineSeq (StyledText Bold defaultAttributeList "**" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "**" :| [Space " ", AlphaNum "c", Symbol "_"]),
      -- Divergence from Asciidoctor: Asciidoctor doesn't respect nesting rule.
      testCase "(*a __b* c__)" $ do
        i <- parseInline "*a __b* c__"
        i `shouldBe` InlineSeq (Symbol "*" :| [AlphaNum "a", Space " ", StyledText Italic defaultAttributeList "__" (AlphaNum "b" :| [Symbol "*", Space " ", AlphaNum "c"]) "__"]),
      testCase "(#*a _b* c_#)" $ do
        i <- parseInline "#*a _b* c_#"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "#" (StyledText Bold (InlineAttributeList "") "*" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "*" :| [Space " ", AlphaNum "c", Symbol "_"]) "#" :| [])
        i `shouldBe` result,
      testCase "(#**a __b** c__#)" $ do
        i <- parseInline "#**a __b** c__#"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "#" (StyledText Bold (InlineAttributeList "") "**" (AlphaNum "a" :| [Space " ", Symbol "_", Symbol "_", AlphaNum "b"]) "**" :| [Space " ", AlphaNum "c", Symbol "_", Symbol "_"]) "#" :| [])
        i `shouldBe` result,
      testCase "(#**a _b** c_#)" $ do
        i <- parseInline "#**a _b** c_#"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "#" (StyledText Bold (InlineAttributeList "") "**" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "**" :| [Space " ", AlphaNum "c", Symbol "_"]) "#" :| [])
        i `shouldBe` result,
      testCase "(#*a __b* c__#)" $ do
        i <- parseInline "#*a __b* c__#"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "#" (Symbol "*" :| [AlphaNum "a", Space " ", StyledText Italic (InlineAttributeList "") "__" (AlphaNum "b" :| [Symbol "*", Space " ", AlphaNum "c"]) "__"]) "#" :| [])
        i `shouldBe` result,
      testCase "(##*a _b* c_##)" $ do
        i <- parseInline "##*a _b* c_##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (StyledText Bold (InlineAttributeList "") "*" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "*" :| [Space " ", AlphaNum "c", Symbol "_"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##**a __b** c__##)" $ do
        i <- parseInline "##**a __b** c__##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (StyledText Bold (InlineAttributeList "") "**" (AlphaNum "a" :| [Space " ", Symbol "_", Symbol "_", AlphaNum "b"]) "**" :| [Space " ", AlphaNum "c", Symbol "_", Symbol "_"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##**a _b** c_##)" $ do
        i <- parseInline "##**a _b** c_##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (StyledText Bold (InlineAttributeList "") "**" (AlphaNum "a" :| [Space " ", Symbol "_", AlphaNum "b"]) "**" :| [Space " ", AlphaNum "c", Symbol "_"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##*a __b* c__##)" $ do
        i <- parseInline "##*a __b* c__##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (Symbol "*" :| [AlphaNum "a", Space " ", StyledText Italic (InlineAttributeList "") "__" (AlphaNum "b" :| [Symbol "*", Space " ", AlphaNum "c"]) "__"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##*a _#b c#* d_##)" $ do
        i <- parseInline "##*a _#b c#* d_##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (StyledText Bold (InlineAttributeList "") "*" (AlphaNum "a" :| [Space " ", Symbol "_", StyledText Custom (InlineAttributeList "") "#" (AlphaNum "b" :| [Space " ", AlphaNum "c"]) "#"]) "*" :| [Space " ", AlphaNum "d", Symbol "_"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##**a __#b c#** d__##)" $ do
        i <- parseInline "##**a __#b c#** d__##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (StyledText Bold (InlineAttributeList "") "**" (AlphaNum "a" :| [Space " ", Symbol "_", Symbol "_", StyledText Custom (InlineAttributeList "") "#" (AlphaNum "b" :| [Space " ", AlphaNum "c"]) "#"]) "**" :| [Space " ", AlphaNum "d", Symbol "_", Symbol "_"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##**a _#b c#** d_##)" $ do
        i <- parseInline "##**a _#b c#** d_##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (StyledText Bold (InlineAttributeList "") "**" (AlphaNum "a" :| [Space " ", Symbol "_", StyledText Custom (InlineAttributeList "") "#" (AlphaNum "b" :| [Space " ", AlphaNum "c"]) "#"]) "**" :| [Space " ", AlphaNum "d", Symbol "_"]) "##" :| [])
        i `shouldBe` result,
      testCase "(##*a __#b c#* d__##)" $ do
        i <- parseInline "##*a __#b c#* d__##"
        let result = InlineSeq (StyledText Custom (InlineAttributeList "") "##" (Symbol "*" :| [AlphaNum "a", Space " ", StyledText Italic (InlineAttributeList "") "__" (StyledText Custom (InlineAttributeList "") "#" (AlphaNum "b" :| [Space " ", AlphaNum "c"]) "#" :| [Symbol "*", Space " ", AlphaNum "d"]) "__"]) "##" :| [])
        i `shouldBe` result
    ]

enclosureInlineAttributeListTests :: TestTree
enclosureInlineAttributeListTests =
  testGroup
    "parameter lists for enclosures"
    [ testCase "simple parameter list for constrained enclosure" $ do
        i <- parseInline "[underline]#a#"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "underline") "#" (AlphaNum "a" :| []) "#" :| []),
      testCase "simple parameter list for unconstrained enclosure" $ do
        i <- parseInline "[underline]##a##"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "underline") "##" (AlphaNum "a" :| []) "##" :| []),
      testCase "parameter list for unconstrained enclosure preceded by word" $ do
        i <- parseInline "a[underline]##b##"
        i `shouldBe` InlineSeq (AlphaNum "a" :| [StyledText Custom (InlineAttributeList "underline") "##" (AlphaNum "b" :| []) "##"]),
      testCase "parameter list for unconstrained enclosure followed by word" $ do
        i <- parseInline "[underline]##a##b"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "underline") "##" (AlphaNum "a" :| []) "##" :| [AlphaNum "b"]),
      testCase "failed parameter list for constrained enclosure preceded by word" $ do
        i <- parseInline "a[underline]#b#"
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Symbol "[", AlphaNum "underline", Symbol "]", StyledText Custom (InlineAttributeList "") "#" (AlphaNum "b" :| []) "#"]),
      testCase "failed parameter list for constrained enclosure followed by word" $ do
        i <- parseInline "[underline]#a#b"
        i `shouldBe` InlineSeq (Symbol "[" :| [AlphaNum "underline", Symbol "]", Symbol "#", AlphaNum "a", Symbol "#", AlphaNum "b"]),
      testCase "unfinished (failed) parameter list" $ do
        i <- parseInline "[underline"
        i `shouldBe` InlineSeq (Symbol "[" :| [AlphaNum "underline"]),
      testCase "isolated (failed) parameter list" $ do
        i <- parseInline "[underline] #a#"
        i `shouldBe` InlineSeq (Symbol "[" :| [AlphaNum "underline", Symbol "]", Space " ", StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| []) "#"])
    ]

punctuationSymbolTests :: TestTree
punctuationSymbolTests =
  testGroup
    "punctuation symbols"
    [ testCase "(a.) sentence finalizer" $ do
        i <- parseInline "a."
        i `shouldBe` InlineSeq (AlphaNum "a" :| [Symbol "."]),
      testCase "(#a.#) sentence finalizer at end of constrained enclosure" $ do
        i <- parseInline "#a.#"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| [Symbol "."]) "#" :| []),
      testCase "(##a.##) sentence finalizer at end of unconstrained enclosure" $ do
        i <- parseInline "##a.##"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "") "##" (AlphaNum "a" :| [Symbol "."]) "##" :| []),
      testCase "(*#a#, b*) comma after enclosure, nested inside another enclosure" $ do
        i <- parseInline "*#a#, b*"
        i `shouldBe` InlineSeq (StyledText Bold (InlineAttributeList "") "*" (StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| []) "#" :| [Symbol ",", Space " ", AlphaNum "b"]) "*" :| []),
      testCase "(#a#,#b#) punctuation symbol separating two enclosures" $ do
        i <- parseInline "#a#,#b#"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| []) "#" :| [Symbol ",", (StyledText Custom (InlineAttributeList "") "#" (AlphaNum "b" :| []) "#")]),
      testCase "(#¿a?#) question marks inside constrained enclosure" $ do
        i <- parseInline "#¿a?#"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "") "#" (Symbol "¿" :| [AlphaNum "a", Symbol "?"]) "#" :| []),
      testCase "(##¿a?##) question marks inside unconstrained enclosure" $ do
        i <- parseInline "##¿a?##"
        i `shouldBe` InlineSeq (StyledText Custom (InlineAttributeList "") "##" (Symbol "¿" :| [AlphaNum "a", Symbol "?"]) "##" :| []),
      testCase "(¿#a#?) question marks outside constrained enclosure" $ do
        i <- parseInline "¿#a#?"
        i `shouldBe` InlineSeq (Symbol "¿" :| [StyledText Custom (InlineAttributeList "") "#" (AlphaNum "a" :| []) "#", Symbol "?"]),
      testCase "(¿##a##?) question marks outside unconstrained enclosure" $ do
        i <- parseInline "¿##a##?"
        i `shouldBe` InlineSeq (Symbol "¿" :| [StyledText Custom (InlineAttributeList "") "##" (AlphaNum "a" :| []) "##", Symbol "?"])
    ]
