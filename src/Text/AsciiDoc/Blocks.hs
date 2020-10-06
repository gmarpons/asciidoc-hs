{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  Text.AsciiDoc.Blocks
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains Parsec-style parsers for AsciiDoc block elements.
--
-- It tries to be compatible with Asciidoctor.
module Text.AsciiDoc.Blocks
  ( -- * AST types
    SectionHeader (..),
    HeaderLevel,
    ListType (..),
    ListCheckStatus (..),
    NestableBlockType (..),
    AdmonitionType (..),
    LiteralBlockType (..),
    LiteralIndentation (..),
    BlockMacroType (..),
    MacroArguments (..),
    IncludeOptions (..),
    AttributeId,
    Comment (..),
    MetadataItem (..),
    Block (..),
    UnparsedInline,
    UnparsedLine (..),

    -- * Parsers
    pDocument,
    pBlocks,
    pBlock,
    pAttributeEntry,
    pBlockId,
    pBlockAttributeList,
    pBlockTitle,
    pNestable,
    pSectionHeader,
    pParagraph,
    pDanglingBlockPrefix,
    pInitialBlankLines,
    pBlankLine,

    -- * Parser type
    State (..),
    Parser,

    -- * Helper low-level parsers
    pLine,
    pLine',
    pLineOneOf,
    pLineNoneOf,
    pInclude,
    pOpenDelimiter,
    pCloseDelimiter,
    satisfyToken,

    -- * Testing
    parseTest,
    parseFile,
    readTokens,
  )
where

import Control.Monad.Combinators hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators.NonEmpty
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.AsciiDoc.Inlines hiding (parseTest)
import qualified Text.AsciiDoc.LineParsers as LP
import qualified Text.Parsec as Parsec
import Text.Parsec.Char (alphaNum, char, satisfy, space)

-- | An explicit header level is necessary, as the output style (e.g. font size)
-- depends on the actual number of @=@'s found (not the actual nesting level).
data SectionHeader a = SectionHeader a HeaderLevel
  deriving (Eq, Show, Functor)

-- | Greater or equal to 0. A section header prefixed by one single "@=@" has
-- level 0, and one with two "@=@"'s has level 1. This follows Asciidoctor
-- behavior.
type HeaderLevel = Int

-- Text: can contain symbols, does not begin nor end with space.
-- Text': can end with spaces.
data ListType
  = Description -- PEG: Space* Text Space* "::" (Space+ Text)? Space*
  | Ordered -- PEG: Space* "."+ Space+ Text'
  | Unordered (Maybe ListCheckStatus) -- PEG: Space* ("*"+ / "-"+ / ... ) Space+ Text'
  | -- | Callouts can be conceptualized as belonging to the block they follow
    -- from, but Asciidoctor treats them as an independent entity, very similar to
    -- any other list.
    Callout -- PEG: "<" (Num / ".") ">" Space+ Text'
  deriving (Eq, Show)

data ListCheckStatus
  = Checked
  | Unchecked
  deriving (Eq, Show)

data NestableBlockType
  = Admonition AdmonitionType
  | Example
  | Sidebar
  | Quote
  | -- | Open block (delimited with "--") with non-standard name.
    Other Text
  deriving (Eq, Show)

data AdmonitionType
  = Note
  | Tip
  | Important
  | Caution
  | Warning
  deriving (Eq, Show)

-- | Literal block types are subject by default to substitution group
-- "Verbatim", if not stated otherwise. The actual substitutions applied can be
-- modified with the @subs@ block attribute, nonetheless.
data LiteralBlockType
  = Fenced
  | Listing
  | Literal LiteralIndentation
  | -- | Default substitution group: None (aka Passthrough).
    Passthrough
  | Source
  | -- | Default substitution group: None (aka Passthrough).
    Stem
  | Verse
  deriving (Eq, Show)

-- | The @Int@ is the indentation of the block. If the @Literal@ block is not
-- signaled by indentation (i.e., @....@ or @[literal]@ is used), then
-- indentation is 0 (all preceding space is copied verbatim as content).
newtype LiteralIndentation = LiteralIndentation Int
  deriving (Eq, Show)

data BlockMacroType
  = ImageBlockMacro
  | TableOfContentsMacro
  | CustomBlockMacro
  deriving (Eq, Show)

data MacroArguments = MacroArguments
  deriving (Eq, Show)

data IncludeOptions
  = IncludeOptions
  deriving (Eq, Show)

type AttributeId = Text

data Comment
  = LineCommentSequence (NonEmpty Text)
  | BlockComment [Text]
  deriving (Eq, Show)

-- | A Block can be preceded by an arbitrary (finite) list of @MetadataItem@s.
--
-- This is a syntactic element. Every value of this type comes from a source
-- line.
data MetadataItem a
  = -- | A block can have more than one ID (aka anchor), and all of them can be
    -- used in cross-references.
    BlockId Text
  | -- | A block can be preceded by any number of @BlockTitle@s (aka labels).
    -- Only the last one is semantically relevant.
    BlockTitle a
  | -- | A block can be preceded by any number of @BlockAttributeList@s. For
    -- positional arguments, only the last list is taken into account.
    --
    -- Some of the elements of the list can be name-value pairs.
    --
    -- TODO. Check if some attributes in the list can contain full inlines, as
    -- it's the case with standalone (aka attribute entry) attributes.
    BlockAttributeList [Text]
  deriving (Eq, Show, Functor)

data BlockPrefixItem a
  = MetadataItem (MetadataItem a)
  | -- | A value of @Nothing@ means the attribute has been unset.
    AttributeEntry AttributeId (Maybe Inline)
  | Comment Comment
  deriving (Eq, Show, Functor)

-- | A Block consists, syntactically, of one or more contiguous and complete
-- lines of text. Some block types can contain other blocks.
data Block a
  = -- | Regular paragraph.
    Paragraph [BlockPrefixItem UnparsedInline] a
  | -- | This data constructor is not used during parsing, it requires an
    -- additional "nesting" pass.
    --
    -- There can be a @Section@ inside an, e.g., open block, but it needs to
    -- have the "discrete" attribute.
    Section [BlockPrefixItem UnparsedInline] (SectionHeader a) [Block a]
  | -- |
    SectionHeaderBlock [BlockPrefixItem UnparsedInline] (SectionHeader a)
  | List ListType [BlockPrefixItem UnparsedInline] (NonEmpty (NonEmpty (Block a)))
  | Table {- TODO. Many things here -}
  | ThematicBreak [BlockPrefixItem UnparsedInline]
  | PageBreak [BlockPrefixItem UnparsedInline]
  | -- | Sequence of blocks of some defined type that allows nested blocks
    -- inside (i.e. admonition, sidebar, example, quote, and open block with no
    -- other standard type).
    Nestable NestableBlockType [BlockPrefixItem UnparsedInline {-State-}] [Block a] -- State
  | VerseBlock [BlockPrefixItem UnparsedInline] [a]
  | -- | Block type determines substitution group applied: @Verbatim@ or @None@
    -- (aka passthrough).
    --
    -- TODO: Check that designed pipeline guarantees that pre-processor
    -- directives are expanded (if not escaped) even in literal blocks, as
    -- https://asciidoctor.org/docs/user-manual/#include-processing states.
    LiteralBlock LiteralBlockType [BlockPrefixItem UnparsedInline] [Text]
  | -- | Some macros accept block metadata, as e.g. @toc::[]@, that accepts
    -- defining its title with @.TITLE@ syntax.
    BlockMacro BlockMacroType [BlockPrefixItem UnparsedInline] MacroArguments
  | DanglingBlockPrefix [BlockPrefixItem UnparsedInline]
  deriving (Eq, Show, Functor)

type UnparsedInline = NonEmpty UnparsedLine

data UnparsedLine
  = TextLine Text
  | CommentLine Text
  deriving (Eq, Show)

data State = State
  { openBlocks :: [(Int, Char)],
    env :: Map.Map AttributeId Inline
  }
  deriving (Eq, Show)

instance Semigroup State where
  x <> y =
    State
      { openBlocks = openBlocks x <> openBlocks y,
        env = env x <> env y
      }

instance Monoid State where
  mempty =
    State
      { openBlocks = mempty,
        env = mempty
      }

type Parser = Parsec.ParsecT [Text] State IO

pDocument :: Parser [Block UnparsedInline]
pDocument = option () pInclude *> pInitialBlankLines *> pBlocks

pBlocks :: Parser [Block UnparsedInline]
pBlocks = many pBlock

pBlock :: Parser (Block UnparsedInline)
pBlock = do
  prefix <- option [] (NE.toList <$> pBlockPrefix)
  pBlock' prefix
  where
    pBlock' prefix =
      pNestable prefix
        <|> pSectionHeader prefix
        <|> pParagraph prefix
        <|> pDanglingBlockPrefix prefix

pBlockPrefix :: Parser (NonEmpty (BlockPrefixItem UnparsedInline))
pBlockPrefix = some pBlockPrefixItem
  where
    pBlockPrefixItem =
      Comment <$> pBlockComment
        <|> Comment <$> pLineCommentSequence
        <|> pAttributeEntry
        <|> pBlockId
        <|> pBlockAttributeList
        <|> pBlockTitle

pBlockComment :: Parser Comment
pBlockComment = do
  delimiter <- choice $ fmap pLine' $ LP.runOfN 4 ['/']
  let n = T.length delimiter
  -- We use here an alternative version of pLine, called pLine', that does not
  -- try to handle pre-processor directives, as includes have no effect inside
  -- block comments.
  ts <-
    manyTill (pLine' LP.anyRemainder) $
      eitherP (pLine' (LP.count n (char '/'))) Parsec.eof
  option () pInclude
  _ <- many pBlankLine
  pure $ BlockComment ts

pLineCommentSequence :: Parser Comment
pLineCommentSequence =
  LineCommentSequence <$> some pLineComment <* many pBlankLine

-- | Parses a line starting with *exactly* two '/'s.
pLineComment :: Parser Text
pLineComment =
  pLine (LP.string "//" *> Parsec.notFollowedBy (char '/') *> LP.anyRemainder)

-- TODO. Add attribute continuations.
pAttributeEntry :: Parser (BlockPrefixItem a)
pAttributeEntry = pAttributeEntry' <* many pBlankLine
  where
    pAttributeEntry' = do
      (k, v) <-
        pLine
          ( (,) <$ LP.string ":" <*> LP.some alphaNum
              <* LP.string ":"
              <* LP.some space <*> LP.anyRemainder
          )
      -- TODO. Replace to a general parseInline with a SubstitutionGroup
      -- parameter.
      let v' = parseInline' v
      Parsec.modifyState $ \st -> st {env = Map.insert k v' (env st)}
      pure $ AttributeEntry k $ Just (parseInline' v)

pBlockId :: Parser (BlockPrefixItem a)
pBlockId = pBlockId' <* many pBlankLine
  where
    pBlockId' = (MetadataItem . BlockId) <$> pLine LP.blockId

pBlockAttributeList :: Parser (BlockPrefixItem a)
pBlockAttributeList = pBlockAttributeList' <* many pBlankLine
  where
    pBlockAttributeList' =
      (MetadataItem . BlockAttributeList)
        <$> pLine LP.blockAttributeList

pBlockTitle :: Parser (BlockPrefixItem UnparsedInline)
pBlockTitle = pBlockTitle' <* many pBlankLine
  where
    pBlockTitle' =
      (MetadataItem . BlockTitle . (:| []) . TextLine)
        <$> pLine (LP.string "." *> (LP.satisfy (not . isSpace) <> LP.anyRemainder))

-- | Parses a nestable delimited block.
pNestable :: [BlockPrefixItem UnparsedInline] -> Parser (Block UnparsedInline)
pNestable prefix = do
  {-st1 <- Parsec.getState-}
  delimiter <- pOpenDelimiter ['=', '*']
  {-st2 <- Parsec.getState-}
  bs <- manyTill pBlock $ eitherP pCloseDelimiter Parsec.eof
  _ <- many pBlankLine
  pure $ case delimiter of
    '=' -> Nestable Example prefix {-st1-} bs {-st2-}
    '*' -> Nestable Sidebar prefix {-st1-} bs {-st2-}
    x -> error $ "pNestable: unexpected character '" <> show x <> "'"

-- | Parses a section header and computes its level.
--
-- POST-CONDITION: The computed level is greater or equal to 0. This follows
-- from the fact that 'LP.runOfN 1' can only return texts of length >= 1.
pSectionHeader ::
  [BlockPrefixItem UnparsedInline] ->
  Parser (Block UnparsedInline)
pSectionHeader prefix =
  SectionHeaderBlock <$> pure prefix <*> pSectionHeader' <* many pBlankLine
  where
    pSectionHeader' =
      ( \(marker, value) ->
          SectionHeader (TextLine value :| []) (-1 + T.length marker)
      )
        <$> pLine
          ( (,)
              <$> choice (LP.runOfN 1 ['=']) <* space
                <*> (LP.satisfy (not . isSpace) <> LP.anyRemainder)
          )

pParagraph :: [BlockPrefixItem UnparsedInline] -> Parser (Block UnparsedInline)
pParagraph prefix =
  Paragraph <$> pure prefix <*> pParagraph' <* many pBlankLine
  where
    pParagraph' =
      (:|) <$> pFirst <*> many pFollowing
    pFirst, pFollowing :: Parser UnparsedLine
    pFirst =
      TextLine
        <$> pLineNoneOf
          -- Nestable
          ( LP.runOfN 4 ['=', '*']
              <> [
                   -- Blank line
                   pure ""
                 ]
          )
    -- Line comments (but not block comments!) can be contained in a paragraph.
    pFollowing =
      CommentLine <$> pLineComment
        <|> TextLine
          <$> pLineNoneOf
            -- Nestable | BlockComment
            ( LP.runOfN 4 ['=', '*', '/']
                <> [
                     -- BlockId, starts with "[["
                     Parsec.try LP.blockId,
                     -- BlockAttributeList, starts with "["
                     pure "" <$> LP.blockAttributeList,
                     -- BlankLine
                     pure ""
                   ]
            )

pDanglingBlockPrefix ::
  [BlockPrefixItem UnparsedInline] ->
  Parser (Block UnparsedInline)
pDanglingBlockPrefix [] = empty
pDanglingBlockPrefix prefix =
  DanglingBlockPrefix prefix
    <$ Parsec.lookAhead (pCloseDelimiter <|> Parsec.eof)

pInitialBlankLines :: Parser [Text]
pInitialBlankLines = many pBlankLine

pBlankLine :: Parser Text
pBlankLine = pLine $ pure ""

-- | Argument can be a parser for the beginning of the line. Function checks
-- that the part of the line not parsed is whitespace.
--
-- If the line is parsed successfully, this combinator checks if an include line
-- follows. If that is the case it inserts the corresponding lines into the
-- input stream of the parser.
pLine :: LP.LineParser a -> Parser a
pLine p = do
  result <- pLine' p
  option () pInclude
  pure result

-- | A version of 'pLine' that does not check if the line is followed by an
-- include.
pLine' :: LP.LineParser a -> Parser a
pLine' p = satisfyToken $
  \t -> f $ Parsec.parse (p <* many space <* Parsec.eof) "" t
  where
    f (Right l) = Just l
    f (Left _) = Nothing

-- | @pLineOneOf ps@ accepts any line that consists in syntax described by any
-- parser in @ps@ plus optional space characters.
--
-- This function runs parsers in @ps@ in sequence, with no lookahead. This means
-- that the order in which parsers appear in @ps@ is relevant, and that
-- 'Parsec.try' could be needed in some elements of @ps@ if their recognized
-- languages share some prefix.
--
-- If blank lines need to be accepted, add @pure ""@ as the last element of
-- @ps@.
pLineOneOf :: [LP.LineParser a] -> Parser a
pLineOneOf parsers = do
  result <- pLineOneOf'
  option () pInclude
  pure result
  where
    pLineOneOf' = satisfyToken $
      \t ->
        f $
          Parsec.parse (choice parsers <* many space <* Parsec.eof) "" t
    f (Right l) = Just l
    f (Left _) = Nothing

-- | @pLineNoneOf ps@ accepts any line that does not consist in syntax described
-- by any parser in @ps@ plus optional space characters.
--
-- This function runs parsers in @ps@ in sequence, with no lookahead. This means
-- that the order in which parsers appear in @ps@ is relevant, and that
-- 'Parsec.try' could be needed in some elements of @ps@ if their recognized
-- languages share some prefix.
--
-- If blank lines need to excluded from acceptance, add @pure ""@ as the last
-- element of @ps@.
pLineNoneOf :: [LP.LineParser a] -> Parser Text
pLineNoneOf parsers = do
  result <- pLineNoneOf'
  option () pInclude
  pure result
  where
    pLineNoneOf' = satisfyToken $
      \t ->
        f t $
          Parsec.parse (choice parsers <* many space <* Parsec.eof) "" t
    f _ (Right _) = Nothing
    f t (Left _) = Just t

pInclude :: Parser ()
pInclude = do
  (filename, arguments) <-
    pLine' $
      (,)
        <$ LP.string "include::"
        <*> LP.many (satisfy (/= '[')) <* char '['
        <*> LP.many (satisfy (/= ']')) <* char ']'
  current <- Parsec.getInput
  -- TODO. Read actual file content, this is a stub.
  Parsec.setInput $ ["// (STUB) include::" <> filename <> "[" <> arguments <> "]"] <> current
  -- Recursive call to handle the case in which the first line of the included
  -- file is also an include.
  option () pInclude

pOpenDelimiter :: [Char] -> Parser Char
pOpenDelimiter cs = do
  -- Parsec.lookAhead needed here because in case we fail later on(because the
  -- block is already open) we don't want to consume any input.
  t <- Parsec.lookAhead $ Parsec.try $ pLineOneOf (LP.runOfN 4 cs)
  -- WARNING! Use of PARTIAL FUNCTION 'T.head': 't' is guaranteed not to be
  -- empty because 'LP.runOfN 4' can only return texts with length >= 4.
  let (n, c) = (T.length t, T.head t)
  st <- Parsec.getState
  -- If block is already open (the delimiter is in the stack of open blocks),
  -- we're not opening it again, but fail. In case we don't fail, we consume the
  -- line that was looked ahead above.
  case ((n, c) `elem` openBlocks st) of
    True -> empty
    False -> do
      Parsec.putState (st {openBlocks = (n, c) : openBlocks st})
      -- Consume one token (aka one line of input), and following blanklines
      _ <- pLine $ LP.anyRemainder
      _ <- many pBlankLine
      -- satisfyToken (const $ Just ())
      pure c

pCloseDelimiter :: Parser ()
pCloseDelimiter = do
  st <- Parsec.getState
  case openBlocks st of
    (n, c) : bs -> do
      -- If (n, c) found in openBlocks stack, pop one element. Only consume line
      -- from input (and look for includes) if the found delimiter matches
      -- openBlocks' top.
      _ <-
        pLine (LP.count n (char c))
          <|> Parsec.lookAhead (choice $ fmap (\(n', c') -> pLine' (LP.count n' (char c'))) bs)
      Parsec.putState $ st {openBlocks = drop 1 (openBlocks st)}
    -- In presence of DanglingBlockPrefix'es, we can try to pop from an empty
    -- openBlocks stack
    [] -> pure ()

-- TODO: Add name to source positions (possibly storing current filename when an
-- inline arrives).
--
-- TODO: Fix line numbering in the presence of includes.
satisfyToken :: (Text -> Maybe a) -> Parser a
satisfyToken matcher = Parsec.tokenPrim show updatePos matcher
  where
    updatePos :: Parsec.SourcePos -> Text -> [Text] -> Parsec.SourcePos
    updatePos pos _ _ = Parsec.incSourceLine pos 1

parseTest :: Parser a -> [Text] -> IO (Either Parsec.ParseError a)
parseTest parser tokens =
  Parsec.runParserT parser mempty "" tokens

readTokens :: FilePath -> IO [Text]
readTokens file = do
  t <- T.readFile file
  pure $ T.lines t

parseFile ::
  FilePath ->
  IO (Either Parsec.ParseError [Block UnparsedInline])
parseFile file = do
  tokens <- readTokens file
  parseTest pDocument tokens

-- | Stub until proper inline parsing is implemented.
parseInline' :: Text -> Inline
parseInline' = Word
