module Tests.Metadata
  ( metadataUnitTests,
  )
where

import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Last (..))
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Hspec.Expectations.Pretty
import Test.Tasty
import Test.Tasty.HUnit
import Text.AsciiDoc.Blocks
import Text.AsciiDoc.Inlines (Inline (..))
import Text.AsciiDoc.Metadata

parseBlockPrefix :: [Text] -> IO (NonEmpty (BlockPrefixItem Inline))
parseBlockPrefix t = case parseTest pBlockPrefix t of
  Right prefix -> pure $ fmap (fmap parseInline'') prefix
  Left parseError -> assertFailure $ "Parser fails: " <> show parseError

metadataUnitTests :: TestTree
metadataUnitTests =
  testGroup
    "Metadata unit tests"
    [ testCase "Block title" $ do
        p <- parseBlockPrefix [".Foo"]
        toMetadata p
          `shouldBe` mempty {metadataTitle = Just (Last (InlineSeq (Word "Foo" :| [])))},
      testCase "Standalone block id" $ do
        p <- parseBlockPrefix ["[[Foo]]"]
        toMetadata p
          `shouldBe` mempty {metadataIds = ["Foo"]},
      testCase "Two standalone block ids" $ do
        p <- parseBlockPrefix ["[[Foo]]", "[[Bar]]"]
        toMetadata p
          `shouldBe` mempty {metadataIds = ["Foo", "Bar"]},
      testCase "Standalone block style" $ do
        p <- parseBlockPrefix ["[Foo]"]
        toMetadata p
          `shouldBe` mempty {metadataStyle = Just (Last "Foo")},
      testCase "Standalone block role" $ do
        p <- parseBlockPrefix ["[.Foo]"]
        -- Compatible with how Asciidoctor cleans style when none is specified
        -- in shortand syntax.
        toMetadata p
          `shouldBe` mempty {metadataStyle = Just (Last ""), metadataRoles = ["Foo"]},
      testCase "Positional attributes" $ do
        p <- parseBlockPrefix ["[Foo, Bar, Baz]"]
        toMetadata p
          `shouldBe` mempty
            { metadataStyle = Just (Last "Foo"),
              metadataPositionalAttributes = IntMap.fromList [(2, "Bar"), (3, "Baz")]
            },
      testCase "Named attribute" $ do
        p <- parseBlockPrefix ["[Foo = Bar]"]
        toMetadata p
          `shouldBe` mempty {metadataNamedAttributes = Map.fromList [("Foo", "Bar")]},
      testCase "Standalone option" $ do
        p <- parseBlockPrefix ["[%Foo]"]
        -- Compatible with how Asciidoctor cleans style when none is specified
        -- in shortand syntax.
        toMetadata p
          `shouldBe` mempty {metadataStyle = Just (Last ""), metadataOptions = ["Foo"]},
      testCase "Complex example" $ do
        p <-
          parseBlockPrefix
            [ "[.Foo]",
              "// Comment",
              "[Foo#Foo%Foo.Foo.Bar%%Bar, 'Foo', Foo = Bar]",
              "[role = 'Baz Foo']",
              "[opts = Baz]",
              "",
              "[[Bar]]",
              "[Bar, Foo = Baz, title=Baz, Bar]"
            ]
        toMetadata p
          `shouldBe` mempty
            { metadataStyle = Just (Last "Bar"),
              metadataIds = ["Foo", "Bar"],
              metadataRoles = ["Baz", "Foo"],
              metadataOptions = ["Foo", "", "Bar", "Baz"],
              metadataTitle = Just (Last (Word "Baz")),
              metadataPositionalAttributes = IntMap.fromList [(2, "Foo"), (4, "Bar")],
              metadataNamedAttributes = Map.fromList [("Foo", "Baz")],
              metadataRoleNamedAttribute = Just (Last ["Baz", "Foo"])
            }
    ]
