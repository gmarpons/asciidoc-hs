{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

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
    BlockPrefixItem (..),
    UnparsedBlockPrefix,
    Block (..),

    -- * Parsers
    documentP,
    blocksP,
    blockP,
    blockPrefixP,
    attributeEntryP,
    blockIdP,
    blockAttributeListP,
    blockTitleP,
    nestableP,
    sectionHeaderP,
    paragraphP,
    danglingBlockPrefixP,
    initialBlankLinesP,
    blankLineP,

    -- * Parser type
    Parser,
    State (..),
    blockParserInitialState,

    -- * Helper low-level parsers
    lineP,
    lineP',
    lineOneOfP,
    lineNoneOfP,
    includeP,
    openDelimiterP,
    closeDelimiterP,
    satisfyToken,
  )
where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad.Combinators hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators.NonEmpty
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Semigroup (Last (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.ElementAttributes
import qualified Text.AsciiDoc.LineParsers as LP
import Text.AsciiDoc.Metadata
import Text.AsciiDoc.SpecialChars
import Text.AsciiDoc.UnparsedInline
import Text.Parsec ((<?>))
import qualified Text.Parsec as Parsec
import Text.Parsec.Char (alphaNum, char, space)

-- | Greater or equal than 0.
-- A section header prefixed by one single "@=@" has level 0, and one with two
-- "@=@"'s has level 1.
-- This follows Asciidoctor's behavior.
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
    BlockAttributeList Text
  deriving (Eq, Show, Functor)

instance ToMetadata (MetadataItem UnparsedInline) UnparsedInline where
  toMetadata (BlockId i) = mempty {metadataIds = [i]}
  toMetadata (BlockTitle t) = mempty {metadataTitle = Just $ Last t}
  toMetadata (BlockAttributeList "") = mempty
  toMetadata (BlockAttributeList t) =
    case Parsec.parse attributeListP "" t of
      Right attributes ->
        toMetadata $ PositionedAttribute <$> NE.zip (1 :| [2 ..]) attributes
      Left _ -> error "toMetadata @(MetadataItem UnparsedInline): parse should not fail"

data BlockPrefixItem a
  = MetadataItem (MetadataItem a)
  | -- | A value of @Nothing@ means the attribute has been unset.
    AttributeEntry AttributeId (Maybe a)
  | Comment Comment
  deriving (Eq, Show, Functor)

instance ToMetadata (BlockPrefixItem UnparsedInline) UnparsedInline where
  toMetadata (MetadataItem x) = toMetadata x
  toMetadata (AttributeEntry _ _) = mempty
  toMetadata (Comment _) = mempty

type UnparsedBlockPrefix = [BlockPrefixItem UnparsedInline]

-- | A Block consists, syntactically, of one or more contiguous and complete
-- lines of text. Some block types can contain other blocks.
data Block a
  = -- | Regular paragraph.
    Paragraph UnparsedBlockPrefix a
  | -- | This data constructor is not used during parsing, it requires an
    -- additional "nesting" pass.
    --
    -- There can be a @Section@ inside an, e.g., open block, but it needs to
    -- have style @discrete@.
    Section UnparsedBlockPrefix HeaderLevel a [Block a]
  | -- | A section header contains the same information as a section, except the
    -- contained sequence of blocks.
    --
    -- After the "nesting" pass, all @SectionHeader@s but @discrete@ ones are
    -- converted to proper @Section@s.
    SectionHeader UnparsedBlockPrefix HeaderLevel a
  | List ListType UnparsedBlockPrefix (NonEmpty (NonEmpty (Block a)))
  | {- Table -- TODO. Many things here -}
    {- ThematicBreak UnparsedBlockPrefix -}
    {- PageBreak UnparsedBlockPrefix -}

    -- | Sequence of blocks of some defined type that allows nested blocks
    -- inside (i.e. admonition, sidebar, example, quote, and open block with no
    -- other standard type).
    Nestable NestableBlockType UnparsedBlockPrefix [Block a]
  | {- VerseBlock UnparsedBlockPrefix [a] -}
    {- -- | Block type determines substitution group applied: @Verbatim@ or @None@
    -- (aka passthrough).
    --
    -- TODO: Check that designed pipeline guarantees that pre-processor
    -- directives are expanded (if not escaped) even in literal blocks, as
    -- https://asciidoctor.org/docs/user-manual/#include-processing states.
    LiteralBlock LiteralBlockType UnparsedBlockPrefix [Text] -}

    {- -- | Some macros accept block metadata, as e.g. @toc::[]@, that accepts
    -- defining its title with @.TITLE@ syntax.
    BlockMacro BlockMacroType UnparsedBlockPrefix MacroArguments -}
    DanglingBlockPrefix UnparsedBlockPrefix
  deriving (Eq, Show, Functor)

-- | Custom parser state for the parser for 'Block's.
data State = State
  { -- | A stack of open 'Nestable' blocks.
    -- Innermost element is the top of the stack.
    --
    -- For every nestable block we store:
    --
    -- * The syntactic 'DelimiterChar' used to open the block.
    --   This is what we need to recognize the matching closing delimiter.
    -- * A stack of list item markers previously used in the current (possibly
    --   nested, aka multi-level, list).
    --   If the parser position is not currently on a list, the stack is empty.
    --
    -- The list representing the stack of open nestable blocks is non-empty: at
    -- the bottom of the stack there is always a value representing the
    -- top-level document (defined in 'State's @Monoid@ instance), so a
    -- one-element stack indicates no nestable block has been open.
    openBlocks :: NonEmpty (Marker DelimiterChar, [Marker ListChar]),
    -- | An environment mapping attribute names to their values (i.e. inlines).
    env :: Map.Map AttributeId Text
  }
  deriving (Eq, Show)

blockParserInitialState :: State
blockParserInitialState =
  State
    { -- We use @'*' :* 0@ as an arbitrary value that is always present as the
      -- bottom of the stack.
      openBlocks = (AsteriskD :* 0, []) :| [],
      env = mempty
    }

type Parser m = Parsec.ParsecT [Text] State m

documentP :: Monad m => Parser m [Block UnparsedInline]
documentP = option () includeP *> initialBlankLinesP *> blocksP

blocksP :: Monad m => Parser m [Block UnparsedInline]
blocksP = many (blockP []) <?> "blocks"

blockP :: Monad m => [LP.LineParser Text] -> Parser m (Block UnparsedInline)
blockP extraParagraphFinalizers = do
  prefix <- option [] (NE.toList <$> blockPrefixP)
  blockP' prefix <?> "block"
  where
    blockP' prefix =
      (nestableP prefix <?> "nestable")
        <|> (sectionHeaderP prefix <?> "section header")
        <|> (listP prefix <?> "list")
        <|> (paragraphP prefix extraParagraphFinalizers <?> "paragraph")
        <|> (danglingBlockPrefixP prefix <?> "dangling block prefix")

blockPrefixP :: Monad m => Parser m (NonEmpty (BlockPrefixItem UnparsedInline))
blockPrefixP = some pBlockPrefixItem <?> "block prefix"
  where
    pBlockPrefixItem =
      Comment <$> blockCommentP
        <|> Comment <$> lineCommentSequenceP
        <|> attributeEntryP
        <|> blockIdP
        <|> blockAttributeListP
        <|> blockTitleP

blockCommentP :: Monad m => Parser m Comment
blockCommentP = do
  _ :* n <- choice $ fmap lineP' $ LP.runOfN 4 [SlashC]
  -- We use here an alternative version of lineP, called lineP', that does not
  -- try to handle pre-processor directives, as includes have no effect inside
  -- block comments.
  ts <-
    manyTill (lineP' LP.anyRemainder) $
      eitherP (lineP' (LP.count n SlashC)) Parsec.eof
  option () includeP
  _ <- many blankLineP
  pure $ BlockComment ts
{-# ANN blockCommentP ("HLint: ignore" :: String) #-}

lineCommentSequenceP :: Monad m => Parser m Comment
lineCommentSequenceP =
  LineCommentSequence <$> some lineCommentP <* many blankLineP

-- | Parses a line starting with *exactly* two '/'s.
lineCommentP :: Monad m => Parser m Text
lineCommentP =
  lineP (LP.string "//" *> Parsec.notFollowedBy (char '/') *> LP.anyRemainder)

-- TODO. Add attribute continuations.
attributeEntryP :: Monad m => Parser m (BlockPrefixItem UnparsedInline)
attributeEntryP = attributeEntryP' <* many blankLineP
  where
    attributeEntryP' = do
      (k, v) <-
        lineP
          ( (,) <$ LP.char ':' <*> LP.some alphaNum
              <* LP.char ':'
              <* LP.some space <*> LP.anyRemainder
          )
      Parsec.modifyState $ \st -> st {env = Map.insert k v (env st)}
      pure $ AttributeEntry k $ Just (MarkupLine v :| [])

blockIdP :: Monad m => Parser m (BlockPrefixItem a)
blockIdP = blockIdP' <* many blankLineP
  where
    blockIdP' = MetadataItem . BlockId <$> lineP LP.blockId

blockAttributeListP :: Monad m => Parser m (BlockPrefixItem a)
blockAttributeListP = blockAttributeListP' <* many blankLineP
  where
    blockAttributeListP' =
      MetadataItem . BlockAttributeList
        <$> lineP LP.blockAttributeList

blockTitleP :: Monad m => Parser m (BlockPrefixItem UnparsedInline)
blockTitleP = blockTitleP' <* many blankLineP
  where
    blockTitleP' =
      MetadataItem . BlockTitle . (:| []) . MarkupLine
        <$> lineP (LP.char '.' *> (LP.satisfy (not . isSpace) <> LP.anyRemainder))

-- | Parses a nestable delimited block.
nestableP ::
  Monad m =>
  UnparsedBlockPrefix ->
  Parser m (Block UnparsedInline)
nestableP prefix = do
  c <- openDelimiterP [AsteriskD, EqualsSignD]
  bs <- manyTill (blockP []) $ eitherP closeDelimiterP Parsec.eof
  _ <- many blankLineP
  pure $ case c of
    AsteriskD -> Nestable Sidebar prefix bs
    HyphenD -> error "nestableP: HyphenD case not implemented yet"
    EqualsSignD -> Nestable Example prefix bs

-- | Parses a section header and computes its level.
--
-- __POST-CONDITION__: The computed level is greater or equal than 0.
sectionHeaderP ::
  Monad m =>
  UnparsedBlockPrefix ->
  Parser m (Block UnparsedInline)
sectionHeaderP prefix = do
  -- Post-condition above follows from the fact that 'LP.runOfN 1' can only
  -- return texts of length >= 1.
  -- TODO. Use type-level Nat in 'Marker', so post-condition can be checked by
  -- the compiler.
  state <- Parsec.getState
  case (NE.tail (openBlocks state), style) of
    -- If parser is currently inside a nestable block (tail state.openBlocks is
    -- not null), and the section header we're trying to parse has a style
    -- different from "discrete", this parser must fail (and the text be
    -- considered a regular paragraph).
    (_ : _, Nothing) -> empty
    (_ : _, Just (Last t)) | t /= "discrete" -> empty
    -- In any other case: parse as a section header.
    _ -> do
      (level, text) <- sectionHeaderP'
      _ <- many blankLineP
      pure $ SectionHeader prefix level text
  where
    sectionHeaderP' :: Monad m => Parser m (HeaderLevel, UnparsedInline)
    sectionHeaderP' =
      (\(_c :* n, x) -> (n - 1, MarkupLine x :| []))
        <$> lineP
          ( (,)
              <$> choice (LP.runOfN 1 [EqualsSignH]) <* some space
                <*> (LP.satisfy (not . isSpace) <> LP.anyRemainder)
          )
    style = metadataStyle $ toMetadata @_ @UnparsedInline $ prefix

listP ::
  (Monad m) =>
  UnparsedBlockPrefix ->
  Parser m (Block UnparsedInline)
listP prefix =
  listP' prefix <* many blankLineP
  where
    allUnorderedMarkers = LP.runOfN 1 [AsteriskL, HyphenL]
    listP' prefix' = do
      state <- Parsec.getState
      let allowedMarkers = allUnorderedMarkers
          -- Disallow as markers those markers already in use in the current
          -- list tree of the innermost open block
          disallowedMarkers = snd currentBlock
          (currentBlock, otherBlocks) = NE.head &&& NE.tail $ openBlocks state
      -- Accept item with a new marker
      (marker@(c :* n), firstLine) <-
        itemFirstLineP allowedMarkers disallowedMarkers
      -- Add new marker to the state
      Parsec.setState $
        state
          { openBlocks =
              (fst currentBlock, marker : disallowedMarkers) :| otherBlocks
          }
      -- Complete the first item, using the already parsed first line
      firstItem <-
        itemP firstLine
          <?> "first item " <> T.unpack (fromMarker marker)
      -- Accept items with the same marker of the first item
      nextItems <-
        many
          ( itemFirstLineP [LP.count n c] []
              >>= itemP . snd <?> "item " <> T.unpack (fromMarker marker)
          )
      -- Recover state present at the beginning of the function. Functions like
      -- pItem could have modified it.
      Parsec.setState state
      pure $ List (Unordered Nothing) prefix' (firstItem :| nextItems)
    itemFirstLineP =
      \x -> lineP . itemFirstLine x
    itemFirstLine ::
      [LP.LineParser (Marker ListChar)] ->
      [Marker ListChar] ->
      LP.LineParser (Marker ListChar, Text)
    itemFirstLine allowedMarkers disallowedMarkers = do
      _ <- many space
      marker <- choice allowedMarkers
      if marker `elem` disallowedMarkers
        then empty
        else do
          _ <- some space
          remainder <- LP.satisfy (not . isSpace) <> LP.anyRemainder
          pure (marker, remainder)
    itemP firstLine = do
      -- As we are inside a list, any list marker is a finalizer of the current
      -- item (no blank line needed)
      nextLines <-
        many $
          paragraphContinuationP [snd <$> itemFirstLine allUnorderedMarkers []]
      nextBlocks <-
        option
          []
          ( ((: []) <$> sublistP <?> "sublist")
              <|> catMaybes <$> many (listContinuationP <?> "list continuation")
              <?> "next blocks"
          )
      _ <- many blankLineP
      pure $ Paragraph [] (MarkupLine firstLine :| nextLines) :| nextBlocks
    -- __Divergence DVB001 from Asciidoctor__. Before sublist:
    --
    --     * Full prefix (including attributes and block title) is allowed.
    --
    --     * Any number of blank lines is allowed.
    --
    -- Probably a linter should warn against any block prefix not preceded by
    -- blank lines.
    sublistP = Parsec.try $ do
      _ <- many blankLineP
      prefix' <- option [] (NE.toList <$> blockPrefixP)
      listP prefix'
    -- __Divergence DVB002 from Asciidoctor__: As in classic AsciiDoc, no blank
    -- lines are allowed before the @+@ sign.
    listContinuationP :: Monad m => Parser m (Maybe (Block UnparsedInline))
    listContinuationP =
      lineP (LP.char '+')
        *> optional blankLineP
        *> optional (blockP [snd <$> itemFirstLine allUnorderedMarkers []])

paragraphP ::
  Monad m =>
  UnparsedBlockPrefix ->
  [LP.LineParser Text] ->
  Parser m (Block UnparsedInline)
paragraphP prefix extraFinalizers =
  Paragraph prefix <$> paragraphP' <* many blankLineP
  where
    paragraphP' =
      (:|) <$> firstP <*> many (paragraphContinuationP extraFinalizers <?> "paragraph continuation")
    firstP :: Monad m => Parser m InputLine
    firstP =
      MarkupLine
        <$> lineNoneOfP
          -- Nestable
          ( (fmap fromMarker <$> LP.runOfN 4 [AsteriskD, EqualsSignD])
              <> [
                   -- Blank line
                   pure ""
                 ]
          )

-- Line comments (but not block comments!) can be contained in a paragraph.
paragraphContinuationP :: Monad m => [LP.LineParser Text] -> Parser m InputLine
paragraphContinuationP extraFinalizers =
  CommentLine <$> lineCommentP
    <|> MarkupLine
      <$> lineNoneOfP
        ( fmap Parsec.try extraFinalizers
            -- Nestable
            <> (fmap fromMarker <$> LP.runOfN 4 [AsteriskD, EqualsSignD])
            -- BlockComment
            <> (fmap fromMarker <$> LP.runOfN 4 [SlashC])
            <> [
                 -- BlockId, starts with "[["
                 Parsec.try LP.blockId,
                 -- BlockAttributeList, starts with "["
                 "" <$ LP.blockAttributeList,
                 -- New block introducer, '+'
                 Parsec.try (LP.char '+'),
                 -- BlankLine
                 pure ""
               ]
        )

danglingBlockPrefixP ::
  Monad m =>
  UnparsedBlockPrefix ->
  Parser m (Block UnparsedInline)
danglingBlockPrefixP [] = empty
danglingBlockPrefixP prefix =
  DanglingBlockPrefix prefix
    <$ Parsec.lookAhead (closeDelimiterP <|> Parsec.eof)

initialBlankLinesP :: Monad m => Parser m [Text]
initialBlankLinesP = many blankLineP

blankLineP :: Monad m => Parser m Text
blankLineP = lineP $ pure ""

-- | Argument can be a parser for the beginning of the line. Function checks
-- that the part of the line not parsed is whitespace.
--
-- If the line is parsed successfully, this combinator checks if an include line
-- follows. If that is the case it inserts the corresponding lines into the
-- input stream of the parser.
lineP :: Monad m => LP.LineParser a -> Parser m a
lineP p = do
  result <- lineP' p
  option () includeP
  pure result

-- | A version of 'lineP' that does not check if the line is followed by an
-- include.
lineP' :: Monad m => LP.LineParser a -> Parser m a
lineP' p = satisfyToken $
  \t -> f $ Parsec.parse (p <* many space <* Parsec.eof) "" t
  where
    f (Right l) = Just l
    f (Left _) = Nothing

-- | @lineOneOfP ps@ accepts any line that consists in syntax described by any
-- parser in @ps@ plus optional space characters.
--
-- This function runs parsers in @ps@ in sequence, with no lookahead. This means
-- that the order in which parsers appear in @ps@ is relevant, and that
-- 'Parsec.try' could be needed in some elements of @ps@ if their recognized
-- languages share some prefix.
--
-- If blank lines need to be accepted, add @pure ""@ as the last element of
-- @ps@.
lineOneOfP :: Monad m => [LP.LineParser a] -> Parser m a
lineOneOfP parsers = do
  result <- lineOneOfP'
  option () includeP
  pure result
  where
    lineOneOfP' = satisfyToken $
      \t ->
        f $
          Parsec.parse (choice parsers <* many space <* Parsec.eof) "" t
    f (Right l) = Just l
    f (Left _) = Nothing

-- | @lineNoneOfP ps@ accepts any line that does not consist in syntax described
-- by any parser in @ps@ plus optional space characters.
--
-- This function runs parsers in @ps@ in sequence, with no lookahead. This means
-- that the order in which parsers appear in @ps@ is relevant, and that
-- 'Parsec.try' could be needed in some elements of @ps@ if their recognized
-- languages share some prefix.
--
-- If blank lines need to excluded from acceptance, add @pure ""@ as the last
-- element of @ps@.
lineNoneOfP :: Monad m => [LP.LineParser a] -> Parser m Text
lineNoneOfP parsers = do
  result <- lineNoneOfP'
  option () includeP
  pure result
  where
    lineNoneOfP' = satisfyToken $
      \t ->
        f t $
          Parsec.parse (choice parsers <* many space <* Parsec.eof) "" t
    f _ (Right _) = Nothing
    f t (Left _) = Just t

includeP :: Parser m ()
includeP = empty

-- includeP = do
--   (filename, arguments) <-
--     lineP' $
--       (,)
--         <$ LP.string "include::"
--         <*> LP.many (satisfy (/= '[')) <* char '['
--         <*> LP.many (satisfy (/= ']')) <* char ']'
--   current <- Parsec.getInput
--   -- TODO. Read actual file content, this is a stub.
--   Parsec.setInput $ ["// (STUB) include::" <> filename <> "[" <> arguments <> "]"] <> current
--   -- Recursive call to handle the case in which the first line of the included
--   -- file is also an include.
--   option () includeP

openDelimiterP ::
  Monad m =>
  [SpecialChar DelimiterChar] ->
  Parser m (SpecialChar DelimiterChar)
openDelimiterP cs = do
  -- Parsec.lookAhead needed here because in case we fail later on (because the
  -- block is already open) we don't want to consume any input.
  (c :* n) <- Parsec.lookAhead $ Parsec.try $ lineOneOfP (LP.runOfN 4 cs)
  st <- Parsec.getState
  -- If block is already open (the delimiter is in the stack of open blocks),
  -- we're not opening it again, but fail. In case we don't fail, we consume the
  -- line that was looked ahead above.
  if (c :* n) `elem` (fst <$> openBlocks st)
    then empty
    else
      ( do
          -- Add found delimiter to the stack of open blocks
          Parsec.putState (st {openBlocks = (c :* n, []) <| openBlocks st})
          -- Complete consumption of the token (aka one line of input), and
          -- following blanklines
          _ <- lineP LP.anyRemainder
          _ <- many blankLineP
          pure c
      )

closeDelimiterP :: Monad m => Parser m ()
closeDelimiterP = do
  st <- Parsec.getState
  let (c :* n, _) = NE.head (openBlocks st)
  case NE.tail (openBlocks st) of
    -- In presence of DanglingBlockPrefix'es, we can try to pop from an
    -- openBlocks stack that contains the initial open block only. We do nothing
    -- in this case.
    [] -> pure ()
    b : bs -> do
      -- If c :* n found in openBlocks stack, pop one element. Only consume line
      -- from input (and look for includes) if the found delimiter matches
      -- openBlocks' top.
      _ <-
        lineP (LP.count n c)
          <|> Parsec.lookAhead
            ( choice $
                fmap (\(c' :* n', _) -> lineP' (LP.count n' c')) (b : bs)
            )
      Parsec.putState $ st {openBlocks = b :| bs}

-- TODO: Add name to source positions (possibly storing current filename when an
-- inline arrives).
--
-- TODO: Fix line numbering in the presence of includes.
satisfyToken :: Monad m => (Text -> Maybe a) -> Parser m a
satisfyToken matcher = Parsec.tokenPrim show updatePos matcher
  where
    updatePos :: Parsec.SourcePos -> Text -> [Text] -> Parsec.SourcePos
    updatePos pos _ _ = Parsec.incSourceLine pos 1
{-# ANN satisfyToken ("HLint: ignore" :: String) #-}
