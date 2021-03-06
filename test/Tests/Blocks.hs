{-# LANGUAGE OverloadedStrings #-}

module Tests.Blocks
  ( parseTest,
    blockUnitTests,
  )
where

import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (Last (..))
import Data.Text (Text)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Text.AsciiDoc.Blocks
import Text.AsciiDoc.Metadata
import Text.AsciiDoc.UnparsedInline
import qualified Text.Parsec as Parsec

parseDocument :: [Text] -> IO (Document UnparsedInline)
parseDocument t = case parseTest documentP t of
  Right x -> pure x
  Left parseError -> assertFailure $ "Parser fails: " <> show parseError

parseBlocks :: [Text] -> IO [Block UnparsedInline]
parseBlocks t = case parseTest blocksP t of
  Right x -> pure x
  Left parseError -> assertFailure $ "Parser fails: " <> show parseError

parseTest :: Parser Identity a -> [Text] -> Either Parsec.ParseError a
parseTest parser tokens =
  runIdentity $ Parsec.runParserT parser blockParserInitialState "" tokens

blockUnitTests :: TestTree
blockUnitTests =
  testGroup
    "block unit tests"
    [ documentUnitTests,
      blockCornerCaseUnitTests,
      paragraphUnitTests,
      sectionHeaderUnitTests,
      danglingBlockPrefixUnitTests,
      nestableUnitTests,
      unorderedListUnitTests,
      nestedListsUnitTests,
      listContinuationUnitTests,
      commentUnitTests
    ]

documentUnitTests :: TestTree
documentUnitTests =
  testGroup
    "document unit tests"
    [ testCase "empty document" $ do
        p <-
          parseDocument
            []
        p `shouldBe` Document Nothing [],
      testCase "single document title" $ do
        p <-
          parseDocument
            [ "= Foo"
            ]
        p
          `shouldBe` Document
            (Just (DocumentHeader [] 0 (MarkupLine "Foo" :| [])))
            [],
      testCase "single document title with prefix" $ do
        p <-
          parseDocument
            [ "[[Foo]]",
              "= Bar"
            ]
        p
          `shouldBe` Document
            (Just (DocumentHeader [MetadataItem (BlockId "Foo")] 0 (MarkupLine "Bar" :| [])))
            [],
      testCase "document title with prefix followed by paragraph" $ do
        p <-
          parseDocument
            [ "[[Foo]]",
              "= Bar",
              "Baz"
            ]
        p
          `shouldBe` Document
            (Just (DocumentHeader [MetadataItem (BlockId "Foo")] 0 (MarkupLine "Bar" :| [])))
            [Paragraph [] (MarkupLine "Baz" :| [])],
      testCase "document title followed by empty line and paragraph" $ do
        p <-
          parseDocument
            [ "= Foo",
              "",
              "Bar"
            ]
        p
          `shouldBe` Document
            (Just (DocumentHeader [] 0 (MarkupLine "Foo" :| [])))
            [Paragraph [] (MarkupLine "Bar" :| [])],
      testCase "document title with level 1 followed by paragraph" $ do
        p <-
          parseDocument
            [ "== Foo",
              "Bar"
            ]
        p
          `shouldBe` Document
            (Just (DocumentHeader [] 1 (MarkupLine "Foo" :| [])))
            [Paragraph [] (MarkupLine "Bar" :| [])],
      testCase "document with no title and a paragraph" $ do
        p <-
          parseDocument
            [ "Foo"
            ]
        p
          `shouldBe` Document
            Nothing
            [Paragraph [] (MarkupLine "Foo" :| [])],
      testCase "single dangling block prefix" $ do
        p <- parseDocument ["[[Foo]]"]
        p
          `shouldBe` Document Nothing [DanglingBlockPrefix [MetadataItem (BlockId "Foo")]],
      testCase "dangling block prefix at eof and after empty line" $ do
        p <-
          parseDocument
            [ "",
              "[[Foo]]"
            ]
        p `shouldBe` Document Nothing [DanglingBlockPrefix [MetadataItem (BlockId "Foo")]]
    ]

blockCornerCaseUnitTests :: TestTree
blockCornerCaseUnitTests =
  testGroup
    "block corner case unit tests"
    [ testCase "empty block list" $ do
        p <-
          parseBlocks
            []
        p `shouldBe` [],
      testCase "single empty line" $ do
        p <-
          parseBlocks
            [ ""
            ]
        p `shouldBe` []
    ]

paragraphUnitTests :: TestTree
paragraphUnitTests =
  testGroup
    "paragraph unit tests"
    [ testCase "one line paragraph" $ do
        p <-
          parseBlocks
            [ "Foo"
            ]
        p `shouldBe` [Paragraph [] (MarkupLine "Foo" :| [])],
      testCase "two lines paragraph" $ do
        p <-
          parseBlocks
            [ "Foo",
              "Bar"
            ]
        p `shouldBe` [Paragraph [] (MarkupLine "Foo" :| [MarkupLine "Bar"])],
      testCase "paragraph followed by empty line" $ do
        p <-
          parseBlocks
            [ "Foo",
              ""
            ]
        p `shouldBe` [Paragraph [] (MarkupLine "Foo" :| [])],
      testCase "paragraph with indented following lines" $ do
        p <-
          parseBlocks
            [ "Foo",
              "  Bar",
              " Baz"
            ]
        p `shouldBe` [Paragraph [] (MarkupLine "Foo" :| [MarkupLine "  Bar", MarkupLine " Baz"])],
      testCase "two paragraphs" $ do
        p <-
          parseBlocks
            [ "Foo",
              "",
              "Bar"
            ]
        p
          `shouldBe` [ Paragraph [] (MarkupLine "Foo" :| []),
                       Paragraph [] (MarkupLine "Bar" :| [])
                     ],
      testCase "paragraph with block prefix" $ do
        p <-
          parseBlocks
            [ ".Foo",
              "// Comment",
              "[Foo#Bar%Baz]",
              "Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         [ MetadataItem (BlockTitle (MarkupLine "Foo" :| [])),
                           Comment (LineCommentSequence (" Comment" :| [])),
                           MetadataItem (BlockAttributeList "Foo#Bar%Baz")
                         ]
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "paragraph with block prefix containing empty lines" $ do
        p <-
          parseBlocks
            [ ".Foo",
              "",
              "[Foo#Bar%Baz]",
              "",
              "Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         [ MetadataItem (BlockTitle (MarkupLine "Foo" :| [])),
                           MetadataItem (BlockAttributeList "Foo#Bar%Baz")
                         ]
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "paragraph followed by dangling block prefix" $ do
        p <-
          parseBlocks
            [ "Foo",
              "",
              ".Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (MarkupLine "Foo" :| []),
                       DanglingBlockPrefix
                         [ MetadataItem (BlockTitle (MarkupLine "Foo" :| []))
                         ]
                     ],
      testCase "paragraph with second line resembling block title" $ do
        p <-
          parseBlocks
            [ "Foo",
              ".Bar"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (MarkupLine "Foo" :| [MarkupLine ".Bar"])
                     ]
    ]

sectionHeaderUnitTests :: TestTree
sectionHeaderUnitTests =
  testGroup
    "section header unit tests"
    [ testCase "level 0 section header" $ do
        p <-
          parseBlocks
            [ "= Foo"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         0
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "level 1 section header" $ do
        p <-
          parseBlocks
            [ "== Foo"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         1
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "level 2 section header" $ do
        p <-
          parseBlocks
            [ "=== Foo"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         2
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "section header with two words" $ do
        p <-
          parseBlocks
            [ "= Foo bar"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         0
                         (MarkupLine "Foo bar" :| [])
                     ],
      testCase "section header beginning with space" $ do
        p <-
          parseBlocks
            [ "=  Foo"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         0
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "section header followed by paragraph" $ do
        p <-
          parseBlocks
            [ "= Foo",
              "Bar"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         0
                         (MarkupLine "Foo" :| []),
                       Paragraph [] (MarkupLine "Bar" :| [])
                     ],
      testCase "section header followed by empty line and paragraph" $ do
        p <-
          parseBlocks
            [ "= Foo",
              "",
              "Bar"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         0
                         (MarkupLine "Foo" :| []),
                       Paragraph [] (MarkupLine "Bar" :| [])
                     ],
      testCase "section header with block prefix" $ do
        p <-
          parseBlocks
            [ ".Foo",
              "= Foo"
            ]
        p
          `shouldBe` [ SectionHeader
                         [MetadataItem (BlockTitle (MarkupLine "Foo" :| []))]
                         0
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "section header followed by paragraph with block prefix" $ do
        p <-
          parseBlocks
            [ "= Foo",
              ".Bar",
              "Bar"
            ]
        p
          `shouldBe` [ SectionHeader
                         []
                         0
                         (MarkupLine "Foo" :| []),
                       Paragraph
                         [MetadataItem (BlockTitle (MarkupLine "Bar" :| []))]
                         (MarkupLine "Bar" :| [])
                     ],
      testCase "discrete section header" $ do
        p <-
          parseBlocks
            [ "[discrete]",
              "= Foo"
            ]
        p
          `shouldBe` [ SectionHeader
                         [MetadataItem (BlockAttributeList "discrete")]
                         0
                         (MarkupLine "Foo" :| [])
                     ]
        case p of
          (SectionHeader prefix _ _) : _ ->
            toMetadata prefix
              `shouldBe` (mempty @(Metadata UnparsedInline))
                { metadataStyle =
                    Just
                      ( Last
                          { getLast = "discrete"
                          }
                      )
                }
          _ -> error "test case: discrete section header",
      -- TODO. Must change when indented literal paragraphs are implemented.
      testCase "false section header (space before '=')" $ do
        p <-
          parseBlocks
            [ " = Foo"
            ]
        p `shouldBe` [Paragraph [] (MarkupLine " = Foo" :| [])]
    ]

danglingBlockPrefixUnitTests :: TestTree
danglingBlockPrefixUnitTests =
  testGroup
    "dangling block prefix unit tests"
    [ testCase "dangling block prefix at eof and after paragraph" $ do
        p <-
          parseBlocks
            [ "Foo",
              "[[Foo]]"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (MarkupLine "Foo" :| []),
                       DanglingBlockPrefix [MetadataItem (BlockId "Foo")]
                     ],
      testCase "dangling block prefix at end of example block" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "",
              "[[Bar]]",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Paragraph [] (MarkupLine "Foo" :| []),
                           DanglingBlockPrefix [MetadataItem (BlockId "Bar")]
                         ]
                     ]
    ]

nestableUnitTests :: TestTree
nestableUnitTests =
  testGroup
    "nestable block unit tests"
    [ testCase "simple example block" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [Paragraph [] (MarkupLine "Foo" :| [])]
                     ],
      testCase "simple sidebar block" $ do
        p <-
          parseBlocks
            [ "****",
              "Foo",
              "****"
            ]
        p
          `shouldBe` [ Nestable
                         Sidebar
                         []
                         [Paragraph [] (MarkupLine "Foo" :| [])]
                     ],
      testCase "example block containing two paragraphs" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Paragraph [] (MarkupLine "Foo" :| []),
                           Paragraph [] (MarkupLine "Bar" :| [])
                         ]
                     ],
      testCase "example block with block title" $ do
        p <-
          parseBlocks
            [ ".Foo",
              "====",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         [MetadataItem (BlockTitle (MarkupLine "Foo" :| []))]
                         [Paragraph [] (MarkupLine "Bar" :| [])]
                     ],
      testCase "sidebar nested into example block" $ do
        p <-
          parseBlocks
            [ "====",
              "****",
              "Bar",
              "****",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Nestable
                             Sidebar
                             []
                             [ Paragraph [] (MarkupLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "sidebar nested into example block and following paragraph" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "****",
              "Bar",
              "****",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Paragraph [] (MarkupLine "Foo" :| []),
                           Nestable
                             Sidebar
                             []
                             [ Paragraph [] (MarkupLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "sidebar nested into example block and following empty line" $ do
        p <-
          parseBlocks
            [ "====",
              "",
              "****",
              "Bar",
              "****",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Nestable
                             Sidebar
                             []
                             [ Paragraph [] (MarkupLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "example block nested into example block and following paragraph" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "======",
              "Bar",
              "======",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Paragraph [] (MarkupLine "Foo" :| []),
                           Nestable
                             Example
                             []
                             [ Paragraph [] (MarkupLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "non-closed sidebar nested into example block" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "****",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Paragraph [] (MarkupLine "Foo" :| []),
                           Nestable
                             Sidebar
                             []
                             [ Paragraph [] (MarkupLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "non-closed example block nested into example block" $ do
        p <-
          parseBlocks
            [ "====",
              "Foo",
              "======",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [ Paragraph [] (MarkupLine "Foo" :| []),
                           Nestable
                             Example
                             []
                             [ Paragraph [] (MarkupLine "Bar" :| [])
                             ]
                         ]
                     ]
    ]

unorderedListUnitTests :: TestTree
unorderedListUnitTests =
  testGroup
    "unordered list unit tests"
    [ testCase "simple unordered list using '-' (hyphen)" $ do
        p <-
          parseBlocks
            [ "- Foo",
              "- Bar",
              "- Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (MarkupLine "Bar" :| []) :| [],
                                  Paragraph [] (MarkupLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "simple unordered list using '*' (asterisk)" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "* Bar",
              "* Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (MarkupLine "Bar" :| []) :| [],
                                  Paragraph [] (MarkupLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with irregular indentation" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "  * Bar",
              " * Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (MarkupLine "Bar" :| []) :| [],
                                  Paragraph [] (MarkupLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with indented first item" $ do
        p <-
          parseBlocks
            [ "Foo",
              "",
              "  * Bar",
              "* Baz"
            ]
        p
          `shouldBe` [ Paragraph [] (MarkupLine "Foo" :| []),
                       List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Bar" :| []) :| [])
                             :| [ Paragraph [] (MarkupLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with irregular line spacing" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "",
              "",
              "* Bar",
              "",
              "* Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (MarkupLine "Bar" :| []) :| [],
                                  Paragraph [] (MarkupLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with multi-line paragraphs" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "Bar",
              "* Baz",
              "  Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Foo" :| [MarkupLine "Bar"]) :| [])
                             :| [Paragraph [] (MarkupLine "Baz" :| [MarkupLine "  Qux"]) :| []]
                         )
                     ],
      testCase "unordered list item with literal second paragraph" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "",
              " Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               -- TODO. Add when indented literal paragraphs are
                               -- implemented:
                               --
                               --  :| [QUOTED [] (MarkupLine "Bar" :| [])]
                               :| []
                           )
                             :| []
                         ),
                       -- TODO. Remove when indented literal paragraphs are
                       -- implemented:
                       Paragraph [] (MarkupLine " Bar" :| [])
                     ],
      testCase "two unordered lists separated by a paragraph" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "",
              "Bar",
              "",
              "* Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (MarkupLine "Foo" :| []) :| []) :| []),
                       Paragraph [] (MarkupLine "Bar" :| []),
                       List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (MarkupLine "Baz" :| []) :| [])
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "unordered list followed up by consecutive example block" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "====",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (MarkupLine "Foo" :| []) :| []) :| []),
                       Nestable
                         Example
                         []
                         [Paragraph [] (MarkupLine "Bar" :| [])]
                     ]
    ]

nestedListsUnitTests :: TestTree
nestedListsUnitTests =
  testGroup
    "nested lists unit tests"
    [ testCase "unordered list using '-' (hyphen) nested into list using '*' (asterisk)" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "- Bar",
              "- Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( (Paragraph [] (MarkupLine "Bar" :| []) :| [])
                                          :| [Paragraph [] (MarkupLine "Baz" :| []) :| []]
                                      )
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered lists using increasing number of '*' (asterisk)" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "** Bar",
              "*** Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (MarkupLine "Bar" :| [])
                                            :| [ List
                                                   (Unordered Nothing)
                                                   []
                                                   ((Paragraph [] (MarkupLine "Baz" :| []) :| []) :| [])
                                               ]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered lists with empty lines interspersed" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "",
              "** Bar",
              "",
              "*** Baz",
              "",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (MarkupLine "Bar" :| [])
                                            :| [ List
                                                   (Unordered Nothing)
                                                   []
                                                   ((Paragraph [] (MarkupLine "Baz" :| []) :| []) :| [])
                                               ]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered lists using unordered number of '*' (asterisk)" $ do
        p <-
          parseBlocks
            [ "** Foo",
              "* Bar",
              "*** Baz",
              " ** Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (MarkupLine "Bar" :| [])
                                            :| [ List
                                                   (Unordered Nothing)
                                                   []
                                                   ((Paragraph [] (MarkupLine "Baz" :| []) :| []) :| [])
                                               ]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered list with multi-line paragraph" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "- Bar",
              "Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( (Paragraph [] (MarkupLine "Bar" :| [MarkupLine "Baz"]) :| [])
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "(DVB001) nested unordered list with block prefixes" $ do
        p <-
          parseBlocks
            [ "[.red]",
              ".FooFoo",
              "* Foo",
              "[.blue]",
              ".BarBar",
              "- Bar",
              "[.green]",
              ".BazBaz",
              "- Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         [ MetadataItem (BlockAttributeList ".red"),
                           MetadataItem (BlockTitle (MarkupLine "FooFoo" :| []))
                         ]
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      [ MetadataItem (BlockAttributeList ".blue"),
                                        MetadataItem (BlockTitle (MarkupLine "BarBar" :| []))
                                      ]
                                      ( (Paragraph [] (MarkupLine "Bar" :| []) :| [])
                                          :| []
                                      )
                                  ]
                           )
                             :| []
                         ),
                       List
                         (Unordered Nothing)
                         [ MetadataItem (BlockAttributeList ".green"),
                           MetadataItem (BlockTitle (MarkupLine "BazBaz" :| []))
                         ]
                         ((Paragraph [] (MarkupLine "Baz" :| []) :| []) :| [])
                     ],
      -- Identical result to the previous test case.
      testCase "(DVB001) nested unordered list with block prefixes and some empty lines" $ do
        p <-
          parseBlocks
            [ "[.red]",
              ".FooFoo",
              "",
              "* Foo",
              "[.blue]",
              "",
              ".BarBar",
              "- Bar",
              "",
              "[.green]",
              ".BazBaz",
              "- Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         [ MetadataItem (BlockAttributeList ".red"),
                           MetadataItem (BlockTitle (MarkupLine "FooFoo" :| []))
                         ]
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      [ MetadataItem (BlockAttributeList ".blue"),
                                        MetadataItem (BlockTitle (MarkupLine "BarBar" :| []))
                                      ]
                                      ( (Paragraph [] (MarkupLine "Bar" :| []) :| [])
                                          :| []
                                      )
                                  ]
                           )
                             :| []
                         ),
                       List
                         (Unordered Nothing)
                         [ MetadataItem (BlockAttributeList ".green"),
                           MetadataItem (BlockTitle (MarkupLine "BazBaz" :| []))
                         ]
                         ((Paragraph [] (MarkupLine "Baz" :| []) :| []) :| [])
                     ]
    ]

listContinuationUnitTests :: TestTree
listContinuationUnitTests =
  testGroup
    "list continuation unit tests"
    [ testCase "list continuation (paragraph), followed by another list item" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "+",
              "Bar",
              "* Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [Paragraph [] (MarkupLine "Bar" :| [])]
                           )
                             :| [Paragraph [] (MarkupLine "Baz" :| []) :| []]
                         )
                     ],
      testCase "two list continuations (paragraph), followed by another list item" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "+",
              "Bar",
              "+",
              "Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ Paragraph [] (MarkupLine "Bar" :| []),
                                    Paragraph [] (MarkupLine "Baz" :| [])
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "list continuation (paragraph) with a block prefix" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "+",
              "[.red]",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ Paragraph
                                      [MetadataItem (BlockAttributeList ".red")]
                                      (MarkupLine "Bar" :| [])
                                  ]
                           )
                             :| []
                         )
                     ],
      testCase "list continuation (example block), followed by another list item" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "+",
              "====",
              "Bar",
              "====",
              "* Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [Nestable Example [] [Paragraph [] (MarkupLine "Bar" :| [])]]
                           )
                             :| [Paragraph [] (MarkupLine "Baz" :| []) :| []]
                         )
                     ],
      testCase "list continuation into a nested unordered list" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "** Bar",
              "+",
              "Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (MarkupLine "Bar" :| [])
                                            :| [Paragraph [] (MarkupLine "Baz" :| [])]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (MarkupLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "dangling list continuation marker in outermost list" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "+",
              "", -- Asciidoctor allows this optional empty line here
              "",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (MarkupLine "Foo" :| []) :| []) :| []),
                       Paragraph [] (MarkupLine "Bar" :| [])
                     ],
      testCase "dangling list continuation marker into a nested unordered list" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "** Bar",
              "+",
              "", -- Asciidoctor allows this optional blank line here
              "",
              "Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ((Paragraph [] (MarkupLine "Bar" :| []) :| []) :| [])
                                  ]
                           )
                             :| []
                         ),
                       Paragraph [] (MarkupLine "Baz" :| [])
                     ],
      testCase "(DVB002) broken list continuation attempt in outermost list" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "",
              "+",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (MarkupLine "Foo" :| []) :| []) :| []),
                       Paragraph [] (MarkupLine "+" :| [MarkupLine "Bar"])
                     ],
      testCase "(DVB002) broken list continuation attempt in nested list" $ do
        p <-
          parseBlocks
            [ "* Foo",
              "** Bar",
              "",
              "+",
              "Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (MarkupLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ((Paragraph [] (MarkupLine "Bar" :| []) :| []) :| [])
                                  ]
                           )
                             :| []
                         ),
                       Paragraph [] (MarkupLine "+" :| [MarkupLine "Baz"])
                     ],
      testCase "line break that resembles list continuation" $ do
        p <-
          parseBlocks
            [ "* Foo",
              " +",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         -- TODO. Must be changed when line breaks are
                         -- implemented.
                         ( ( Paragraph
                               []
                               ( MarkupLine "Foo"
                                   :| [ MarkupLine " +",
                                        MarkupLine "Bar"
                                      ]
                               )
                               :| []
                           )
                             :| []
                         )
                     ]
    ]

commentUnitTests :: TestTree
commentUnitTests =
  testGroup
    "comment unit tests"
    [ testCase "dangling block comment" $ do
        p <-
          parseBlocks
            [ "////",
              "Foo",
              "////"
            ]
        p
          `shouldBe` [DanglingBlockPrefix [Comment (BlockComment ["Foo"])]],
      testCase "dangling line comment sequence" $ do
        p <-
          parseBlocks
            [ "//Foo",
              "// Bar"
            ]
        p
          `shouldBe` [ DanglingBlockPrefix
                         [Comment (LineCommentSequence ("Foo" :| [" Bar"]))]
                     ],
      testCase "block comment before paragraph" $ do
        p <-
          parseBlocks
            [ "////",
              "Foo",
              "////",
              "Bar"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (BlockComment ["Foo"])]
                         (MarkupLine "Bar" :| [])
                     ],
      testCase "block comment before paragraph, with redundant space" $ do
        p <-
          parseBlocks
            [ "//// ",
              "Foo",
              "//// ",
              "Bar"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (BlockComment ["Foo"])]
                         (MarkupLine "Bar" :| [])
                     ],
      testCase "block comment before paragraph, separated by blank line" $ do
        p <-
          parseBlocks
            [ "////",
              "Foo",
              "////",
              "",
              "Bar"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (BlockComment ["Foo"])]
                         (MarkupLine "Bar" :| [])
                     ],
      testCase "empty block comment before paragraph" $ do
        p <-
          parseBlocks
            [ "////",
              "////",
              "Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (BlockComment [])]
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "empty line comment before paragraph" $ do
        p <-
          parseBlocks
            [ "//",
              "Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (LineCommentSequence ("" :| []))]
                         (MarkupLine "Foo" :| [])
                     ],
      testCase "block comment with multiple pseudo-paragraphs" $ do
        p <-
          parseBlocks
            [ "////",
              "Foo",
              "",
              "Bar",
              "////",
              "Baz"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (BlockComment ["Foo", "", "Bar"])]
                         (MarkupLine "Baz" :| [])
                     ],
      testCase "line comment sequence before paragraph" $ do
        p <-
          parseBlocks
            [ "//Foo",
              "// Bar",
              "Baz"
            ]
        p
          `shouldBe` [ Paragraph
                         [Comment (LineCommentSequence ("Foo" :| [" Bar"]))]
                         (MarkupLine "Baz" :| [])
                     ],
      testCase "line comment inside paragraph" $ do
        p <-
          parseBlocks
            [ "Foo",
              "// Bar",
              "Baz"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (MarkupLine "Foo" :| [CommentLine " Bar", MarkupLine "Baz"])
                     ],
      testCase "line comment inside paragraph and after paragraph" $ do
        p <-
          parseBlocks
            [ "Foo",
              "// Bar",
              "Baz",
              "//Qux"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         ( MarkupLine "Foo"
                             :| [ CommentLine " Bar",
                                  MarkupLine "Baz",
                                  CommentLine "Qux"
                                ]
                         )
                     ],
      testCase "line comment sequence before section header" $ do
        p <-
          parseBlocks
            [ "//Foo",
              "// Bar",
              "== Baz"
            ]
        p
          `shouldBe` [ SectionHeader
                         [Comment (LineCommentSequence ("Foo" :| [" Bar"]))]
                         1
                         (MarkupLine "Baz" :| [])
                     ],
      testCase "block comment followed by line comment sequence" $ do
        p <-
          parseBlocks
            [ "////",
              "Foo",
              "",
              "////",
              "//Bar",
              "Baz"
            ]
        p
          `shouldBe` [ Paragraph
                         [ Comment (BlockComment ["Foo", ""]),
                           Comment (LineCommentSequence ("Bar" :| []))
                         ]
                         (MarkupLine "Baz" :| [])
                     ],
      testCase "line comment sequence followed by block comment" $ do
        p <-
          parseBlocks
            [ "// Foo",
              "//Bar",
              "////",
              "Baz",
              "////",
              "Qux"
            ]
        p
          `shouldBe` [ Paragraph
                         [ Comment (LineCommentSequence (" Foo" :| ["Bar"])),
                           Comment (BlockComment ["Baz"])
                         ]
                         (MarkupLine "Qux" :| [])
                     ],
      testCase "block comment with more than four '/' (slash)" $ do
        p <-
          parseBlocks
            [ "/////",
              "Foo",
              "////",
              "Bar",
              "////",
              "/////",
              "Baz"
            ]
        p
          `shouldBe` [ Paragraph
                         [ Comment (BlockComment ["Foo", "////", "Bar", "////"])
                         ]
                         (MarkupLine "Baz" :| [])
                     ],
      testCase "dangling non-closed block comment" $ do
        p <-
          parseBlocks
            [ "////",
              "Foo",
              "",
              "Bar"
            ]
        p
          `shouldBe` [ DanglingBlockPrefix
                         [Comment (BlockComment ["Foo", "", "Bar"])]
                     ],
      testCase "bad block comment opening, with three '/' (slashes)" $ do
        p <-
          parseBlocks
            [ "///",
              "Foo",
              "////"
            ]
        p
          `shouldBe` [ Paragraph [] (MarkupLine "///" :| [MarkupLine "Foo"]),
                       DanglingBlockPrefix [Comment (BlockComment [])]
                     ],
      testCase "bad line comment, with three '/' (slashes)" $ do
        p <-
          parseBlocks
            [ "///Foo",
              "Bar"
            ]
        p
          `shouldBe` [Paragraph [] (MarkupLine "///Foo" :| [MarkupLine "Bar"])],
      testCase "bad block comment opening, preceded by space" $ do
        p <-
          parseBlocks
            [ " ////",
              "Foo",
              "////"
            ]
        p
          `shouldBe` [ Paragraph [] (MarkupLine " ////" :| [MarkupLine "Foo"]),
                       DanglingBlockPrefix [Comment (BlockComment [])]
                     ],
      testCase "bad line comment opening, preceded by space" $ do
        p <-
          parseBlocks
            [ " //Foo",
              "Bar"
            ]
        p
          `shouldBe` [Paragraph [] (MarkupLine " //Foo" :| [MarkupLine "Bar"])]
    ]
