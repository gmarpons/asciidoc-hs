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
import qualified Text.Parsec as Parsec

parseTest :: Parser Identity a -> [Text] -> Either Parsec.ParseError a
parseTest parser tokens =
  runIdentity $ Parsec.runParserT parser mempty "" tokens

parseDocument :: [Text] -> IO [Block UnparsedInline]
parseDocument t = case parseTest pDocument t of
  Right prefix -> pure prefix
  Left parseError -> assertFailure $ "Parser fails: " <> show parseError

blockUnitTests :: TestTree
blockUnitTests =
  testGroup
    "block unit tests"
    [ paragraphUnitTests,
      sectionHeaderUnitTests,
      danglingBlockPrefixUnitTests,
      nestableUnitTests,
      unorderedListUnitTests,
      nestedListsUnitTests,
      listContinuationUnitTests
    ]

paragraphUnitTests :: TestTree
paragraphUnitTests =
  testGroup
    "paragraph unit tests"
    [ testCase "one line paragraph" $ do
        p <-
          parseDocument
            [ "Foo"
            ]
        p `shouldBe` [Paragraph [] (TextLine "Foo" :| [])],
      testCase "two lines paragraph" $ do
        p <-
          parseDocument
            [ "Foo",
              "Bar"
            ]
        p `shouldBe` [Paragraph [] (TextLine "Foo" :| [TextLine "Bar"])],
      testCase "paragraph followed by blank line" $ do
        p <-
          parseDocument
            [ "Foo",
              ""
            ]
        p `shouldBe` [Paragraph [] (TextLine "Foo" :| [])],
      testCase "paragraph with indented following lines" $ do
        p <-
          parseDocument
            [ "Foo",
              "  Bar",
              " Baz"
            ]
        p `shouldBe` [Paragraph [] (TextLine "Foo" :| [TextLine "  Bar", TextLine " Baz"])],
      testCase "two paragraphs" $ do
        p <-
          parseDocument
            [ "Foo",
              "",
              "Bar"
            ]
        p
          `shouldBe` [ Paragraph [] (TextLine "Foo" :| []),
                       Paragraph [] (TextLine "Bar" :| [])
                     ],
      testCase "paragraph with block prefix" $ do
        p <-
          parseDocument
            [ ".Foo",
              "// Comment",
              "[Foo#Bar%Baz]",
              "Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         [ MetadataItem (BlockTitle (TextLine "Foo" :| [])),
                           Comment (LineCommentSequence (" Comment" :| [])),
                           MetadataItem (BlockAttributeList "Foo#Bar%Baz")
                         ]
                         (TextLine "Foo" :| [])
                     ],
      testCase "paragraph with block prefix containing blank lines" $ do
        p <-
          parseDocument
            [ ".Foo",
              "",
              "[Foo#Bar%Baz]",
              "",
              "Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         [ MetadataItem (BlockTitle (TextLine "Foo" :| [])),
                           MetadataItem (BlockAttributeList "Foo#Bar%Baz")
                         ]
                         (TextLine "Foo" :| [])
                     ],
      testCase "paragraph followed by dangling block prefix" $ do
        p <-
          parseDocument
            [ "Foo",
              "",
              ".Foo"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (TextLine "Foo" :| []),
                       DanglingBlockPrefix
                         [ MetadataItem (BlockTitle (TextLine "Foo" :| []))
                         ]
                     ],
      testCase "paragraph with second line resembling block title" $ do
        p <-
          parseDocument
            [ "Foo",
              ".Bar"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (TextLine "Foo" :| [TextLine ".Bar"])
                     ]
    ]

sectionHeaderUnitTests :: TestTree
sectionHeaderUnitTests =
  testGroup
    "section header unit tests"
    [ testCase "level 0 section header" $ do
        p <-
          parseDocument
            [ "= Foo"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         )
                     ],
      testCase "level 1 section header" $ do
        p <-
          parseDocument
            [ "== Foo"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 1
                         )
                     ],
      testCase "level 2 section header" $ do
        p <-
          parseDocument
            [ "=== Foo"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 2
                         )
                     ],
      testCase "section header with two words" $ do
        p <-
          parseDocument
            [ "= Foo bar"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo bar" :| []) 0
                         )
                     ],
      testCase "section header beginning with space" $ do
        p <-
          parseDocument
            [ "=  Foo"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         )
                     ],
      testCase "section header followed by paragraph" $ do
        p <-
          parseDocument
            [ "= Foo",
              "Bar"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         ),
                       Paragraph [] (TextLine "Bar" :| [])
                     ],
      testCase "section header followed by blank line and paragraph" $ do
        p <-
          parseDocument
            [ "= Foo",
              "",
              "Bar"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         ),
                       Paragraph [] (TextLine "Bar" :| [])
                     ],
      testCase "section header with block prefix" $ do
        p <-
          parseDocument
            [ ".Foo",
              "= Foo"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         [MetadataItem (BlockTitle (TextLine "Foo" :| []))]
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         )
                     ],
      testCase "section header followed by paragraph with block prefix" $ do
        p <-
          parseDocument
            [ "= Foo",
              ".Bar",
              "Bar"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         []
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         ),
                       Paragraph
                         [MetadataItem (BlockTitle (TextLine "Bar" :| []))]
                         (TextLine "Bar" :| [])
                     ],
      testCase "discrete section header" $ do
        p <-
          parseDocument
            [ "[discrete]",
              "= Foo"
            ]
        p
          `shouldBe` [ SectionHeaderBlock
                         [MetadataItem (BlockAttributeList "discrete")]
                         ( SectionHeader (TextLine "Foo" :| []) 0
                         )
                     ]
        let (SectionHeaderBlock (m : _) _) : _ = p
        toMetadata (fmap parseInline'' m)
          `shouldBe` mempty
            { metadataStyle =
                Just
                  ( Last
                      { getLast = "discrete"
                      }
                  )
            },
      -- TODO. Must change when indented literal paragraphs are implemented.
      testCase "false section header (space before '=')" $ do
        p <-
          parseDocument
            [ " = Foo"
            ]
        p `shouldBe` [Paragraph [] (TextLine " = Foo" :| [])]
    ]

danglingBlockPrefixUnitTests :: TestTree
danglingBlockPrefixUnitTests =
  testGroup
    "dangling block prefix unit tests"
    [ testCase "single dangling block prefix" $ do
        p <- parseDocument ["[[Foo]]"]
        p `shouldBe` [DanglingBlockPrefix [MetadataItem (BlockId "Foo")]],
      testCase "dangling block prefix at eof and after blank line" $ do
        p <-
          parseDocument
            [ "",
              "[[Foo]]"
            ]
        p `shouldBe` [DanglingBlockPrefix [MetadataItem (BlockId "Foo")]],
      testCase "dangling block prefix at eof and after paragraph" $ do
        p <-
          parseDocument
            [ "Foo",
              "[[Foo]]"
            ]
        p
          `shouldBe` [ Paragraph
                         []
                         (TextLine "Foo" :| []),
                       DanglingBlockPrefix [MetadataItem (BlockId "Foo")]
                     ],
      testCase "dangling block prefix at end of example block" $ do
        p <-
          parseDocument
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
                         [ Paragraph [] (TextLine "Foo" :| []),
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
          parseDocument
            [ "====",
              "Foo",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         []
                         [Paragraph [] (TextLine "Foo" :| [])]
                     ],
      testCase "simple sidebar block" $ do
        p <-
          parseDocument
            [ "****",
              "Foo",
              "****"
            ]
        p
          `shouldBe` [ Nestable
                         Sidebar
                         []
                         [Paragraph [] (TextLine "Foo" :| [])]
                     ],
      testCase "example block containing two paragraphs" $ do
        p <-
          parseDocument
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
                         [ Paragraph [] (TextLine "Foo" :| []),
                           Paragraph [] (TextLine "Bar" :| [])
                         ]
                     ],
      testCase "example block with block title" $ do
        p <-
          parseDocument
            [ ".Foo",
              "====",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ Nestable
                         Example
                         [MetadataItem (BlockTitle (TextLine "Foo" :| []))]
                         [Paragraph [] (TextLine "Bar" :| [])]
                     ],
      testCase "sidebar nested into example block" $ do
        p <-
          parseDocument
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
                             [ Paragraph [] (TextLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "sidebar nested into example block and following paragraph" $ do
        p <-
          parseDocument
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
                         [ Paragraph [] (TextLine "Foo" :| []),
                           Nestable
                             Sidebar
                             []
                             [ Paragraph [] (TextLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "sidebar nested into example block and following blank line" $ do
        p <-
          parseDocument
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
                             [ Paragraph [] (TextLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "example block nested into example block and following paragraph" $ do
        p <-
          parseDocument
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
                         [ Paragraph [] (TextLine "Foo" :| []),
                           Nestable
                             Example
                             []
                             [ Paragraph [] (TextLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "unfinished sidebar nested into example block" $ do
        p <-
          parseDocument
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
                         [ Paragraph [] (TextLine "Foo" :| []),
                           Nestable
                             Sidebar
                             []
                             [ Paragraph [] (TextLine "Bar" :| [])
                             ]
                         ]
                     ],
      testCase "unfinished example block nested into example block" $ do
        p <-
          parseDocument
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
                         [ Paragraph [] (TextLine "Foo" :| []),
                           Nestable
                             Example
                             []
                             [ Paragraph [] (TextLine "Bar" :| [])
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
          parseDocument
            [ "- Foo",
              "- Bar",
              "- Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (TextLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (TextLine "Bar" :| []) :| [],
                                  Paragraph [] (TextLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "simple unordered list using '*' (asterisk)" $ do
        p <-
          parseDocument
            [ "* Foo",
              "* Bar",
              "* Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (TextLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (TextLine "Bar" :| []) :| [],
                                  Paragraph [] (TextLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with irregular indentation" $ do
        p <-
          parseDocument
            [ "* Foo",
              "  * Bar",
              " * Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (TextLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (TextLine "Bar" :| []) :| [],
                                  Paragraph [] (TextLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with indented first item" $ do
        p <-
          parseDocument
            [ "Foo",
              "",
              "  * Bar",
              "* Baz"
            ]
        p
          `shouldBe` [ Paragraph [] (TextLine "Foo" :| []),
                       List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (TextLine "Bar" :| []) :| [])
                             :| [ Paragraph [] (TextLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with irregular line spacing" $ do
        p <-
          parseDocument
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
                         ( (Paragraph [] (TextLine "Foo" :| []) :| [])
                             :| [ Paragraph [] (TextLine "Bar" :| []) :| [],
                                  Paragraph [] (TextLine "Baz" :| []) :| []
                                ]
                         )
                     ],
      testCase "unordered list with multi-line paragraphs" $ do
        p <-
          parseDocument
            [ "* Foo",
              "Bar",
              "* Baz",
              "  Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (TextLine "Foo" :| [TextLine "Bar"]) :| [])
                             :| [Paragraph [] (TextLine "Baz" :| [TextLine "  Qux"]) :| []]
                         )
                     ],
      testCase "unordered list item with literal second paragraph" $ do
        p <-
          parseDocument
            [ "* Foo",
              "",
              " Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               -- TODO. Add when indented literal paragraphs are
                               -- implemented:
                               --
                               --  :| [QUOTED [] (TextLine "Bar" :| [])]
                               :| []
                           )
                             :| []
                         ),
                       -- TODO. Remove when indented literal paragraphs are
                       -- implemented:
                       Paragraph [] (TextLine " Bar" :| [])
                     ],
      testCase "two unordered lists separated by a paragraph" $ do
        p <-
          parseDocument
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
                         ((Paragraph [] (TextLine "Foo" :| []) :| []) :| []),
                       Paragraph [] (TextLine "Bar" :| []),
                       List
                         (Unordered Nothing)
                         []
                         ( (Paragraph [] (TextLine "Baz" :| []) :| [])
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "unordered list followed up by consecutive example block" $ do
        p <-
          parseDocument
            [ "* Foo",
              "====",
              "Bar",
              "===="
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (TextLine "Foo" :| []) :| []) :| []),
                       Nestable
                         Example
                         []
                         [Paragraph [] (TextLine "Bar" :| [])]
                     ]
    ]

nestedListsUnitTests :: TestTree
nestedListsUnitTests =
  testGroup
    "nested lists unit tests"
    [ testCase "unordered list using '-' (hyphen) nested into list using '*' (asterisk)" $ do
        p <-
          parseDocument
            [ "* Foo",
              "- Bar",
              "- Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( (Paragraph [] (TextLine "Bar" :| []) :| [])
                                          :| [Paragraph [] (TextLine "Baz" :| []) :| []]
                                      )
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered lists using increasing number of '*' (asterisk)" $ do
        p <-
          parseDocument
            [ "* Foo",
              "** Bar",
              "*** Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (TextLine "Bar" :| [])
                                            :| [ List
                                                   (Unordered Nothing)
                                                   []
                                                   ((Paragraph [] (TextLine "Baz" :| []) :| []) :| [])
                                               ]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered lists with blank lines interspersed" $ do
        p <-
          parseDocument
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
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (TextLine "Bar" :| [])
                                            :| [ List
                                                   (Unordered Nothing)
                                                   []
                                                   ((Paragraph [] (TextLine "Baz" :| []) :| []) :| [])
                                               ]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered lists using unordered number of '*' (asterisk)" $ do
        p <-
          parseDocument
            [ "** Foo",
              "* Bar",
              "*** Baz",
              " ** Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (TextLine "Bar" :| [])
                                            :| [ List
                                                   (Unordered Nothing)
                                                   []
                                                   ((Paragraph [] (TextLine "Baz" :| []) :| []) :| [])
                                               ]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "nested unordered list with multi-line paragraph" $ do
        p <-
          parseDocument
            [ "* Foo",
              "- Bar",
              "Baz",
              "* Qux"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( (Paragraph [] (TextLine "Bar" :| [TextLine "Baz"]) :| [])
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "(DVB001) nested unordered list with block prefixes" $ do
        p <-
          parseDocument
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
                           MetadataItem (BlockTitle (TextLine "FooFoo" :| []))
                         ]
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      [ MetadataItem (BlockAttributeList ".blue"),
                                        MetadataItem (BlockTitle (TextLine "BarBar" :| []))
                                      ]
                                      ( (Paragraph [] (TextLine "Bar" :| []) :| [])
                                          :| []
                                      )
                                  ]
                           )
                             :| []
                         ),
                       List
                         (Unordered Nothing)
                         [ MetadataItem (BlockAttributeList ".green"),
                           MetadataItem (BlockTitle (TextLine "BazBaz" :| []))
                         ]
                         ((Paragraph [] (TextLine "Baz" :| []) :| []) :| [])
                     ],
      -- Identical result to the previous test case.
      testCase "(DVB001) nested unordered list with block prefixes and some blank lines" $ do
        p <-
          parseDocument
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
                           MetadataItem (BlockTitle (TextLine "FooFoo" :| []))
                         ]
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      [ MetadataItem (BlockAttributeList ".blue"),
                                        MetadataItem (BlockTitle (TextLine "BarBar" :| []))
                                      ]
                                      ( (Paragraph [] (TextLine "Bar" :| []) :| [])
                                          :| []
                                      )
                                  ]
                           )
                             :| []
                         ),
                       List
                         (Unordered Nothing)
                         [ MetadataItem (BlockAttributeList ".green"),
                           MetadataItem (BlockTitle (TextLine "BazBaz" :| []))
                         ]
                         ((Paragraph [] (TextLine "Baz" :| []) :| []) :| [])
                     ]
    ]

listContinuationUnitTests :: TestTree
listContinuationUnitTests =
  testGroup
    "list continuation unit tests"
    [ testCase "list continuation (paragraph), followed by another list item" $ do
        p <-
          parseDocument
            [ "* Foo",
              "+",
              "Bar",
              "* Baz"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [Paragraph [] (TextLine "Bar" :| [])]
                           )
                             :| [Paragraph [] (TextLine "Baz" :| []) :| []]
                         )
                     ],
      testCase "two list continuations (paragraph), followed by another list item" $ do
        p <-
          parseDocument
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
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ Paragraph [] (TextLine "Bar" :| []),
                                    Paragraph [] (TextLine "Baz" :| [])
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "list continuation (paragraph) with a block prefix" $ do
        p <-
          parseDocument
            [ "* Foo",
              "+",
              "[.red]",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ Paragraph
                                      [MetadataItem (BlockAttributeList ".red")]
                                      (TextLine "Bar" :| [])
                                  ]
                           )
                             :| []
                         )
                     ],
      testCase "list continuation (example block), followed by another list item" $ do
        p <-
          parseDocument
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
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [Nestable Example [] [Paragraph [] (TextLine "Bar" :| [])]]
                           )
                             :| [Paragraph [] (TextLine "Baz" :| []) :| []]
                         )
                     ],
      testCase "list continuation into a nested unordered list" $ do
        p <-
          parseDocument
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
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ( ( Paragraph [] (TextLine "Bar" :| [])
                                            :| [Paragraph [] (TextLine "Baz" :| [])]
                                        )
                                          :| []
                                      )
                                  ]
                           )
                             :| [Paragraph [] (TextLine "Qux" :| []) :| []]
                         )
                     ],
      testCase "dangling list continuation marker in outermost list" $ do
        p <-
          parseDocument
            [ "* Foo",
              "+",
              "", -- Asciidoctor allows this optional blank line here
              "",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (TextLine "Foo" :| []) :| []) :| []),
                       Paragraph [] (TextLine "Bar" :| [])
                     ],
      testCase "dangling list continuation marker into a nested unordered list" $ do
        p <-
          parseDocument
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
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ((Paragraph [] (TextLine "Bar" :| []) :| []) :| [])
                                  ]
                           )
                             :| []
                         ),
                       Paragraph [] (TextLine "Baz" :| [])
                     ],
      testCase "(DVB002) broken list continuation attempt in outermost list" $ do
        p <-
          parseDocument
            [ "* Foo",
              "",
              "+",
              "Bar"
            ]
        p
          `shouldBe` [ List
                         (Unordered Nothing)
                         []
                         ((Paragraph [] (TextLine "Foo" :| []) :| []) :| []),
                       Paragraph [] (TextLine "+" :| [TextLine "Bar"])
                     ],
      testCase "(DVB002) broken list continuation attempt in nested list" $ do
        p <-
          parseDocument
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
                         ( ( Paragraph [] (TextLine "Foo" :| [])
                               :| [ List
                                      (Unordered Nothing)
                                      []
                                      ((Paragraph [] (TextLine "Bar" :| []) :| []) :| [])
                                  ]
                           )
                             :| []
                         ),
                       Paragraph [] (TextLine "+" :| [TextLine "Baz"])
                     ],
      testCase "line break that resembles list continuation" $ do
        p <-
          parseDocument
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
                               ( TextLine "Foo"
                                   :| [ TextLine " +",
                                        TextLine "Bar"
                                      ]
                               )
                               :| []
                           )
                             :| []
                         )
                     ]
    ]
