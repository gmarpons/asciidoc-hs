{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

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
    BlockPrefixItem (..),
    Block (..),
    UnparsedInline,
    UnparsedLine (..),

    -- * Parsers
    pDocument,
    pBlocks,
    pBlock,
    pBlockPrefix,
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
    parseInline'',
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
import Data.Maybe (catMaybes, mapMaybe)
import Data.Semigroup (Last (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.AsciiDoc.Attributes as Attributes
import Text.AsciiDoc.Inlines hiding (Parser)
import qualified Text.AsciiDoc.LineParsers as LP
import Text.AsciiDoc.Metadata
import Text.AsciiDoc.SpecialChars
import Text.Parsec ((<?>))
import qualified Text.Parsec as Parsec
import Text.Parsec.Char (alphaNum, char, space)

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
    BlockAttributeList Text
  deriving (Eq, Show, Functor)

instance ToMetadata (MetadataItem Inline) where
  toMetadata (BlockId i) = mempty {metadataIds = [i]}
  toMetadata (BlockTitle t) = mempty {metadataTitle = Just $ Last t}
  toMetadata (BlockAttributeList "") = mempty
  toMetadata (BlockAttributeList t) =
    case Parsec.parse Attributes.pAttributeList "" t of
      Right attributes ->
        toMetadata $ NE.zip (1 :| [2 ..] :: NonEmpty Int) attributes
      Left _ -> error "toMetadata @(MetadataItem Inline): parse should not fail"

data BlockPrefixItem a
  = MetadataItem (MetadataItem a)
  | -- | A value of @Nothing@ means the attribute has been unset.
    AttributeEntry AttributeId (Maybe Inline)
  | Comment Comment
  deriving (Eq, Show, Functor)

instance ToMetadata (BlockPrefixItem Inline) where
  toMetadata (MetadataItem x) = toMetadata x
  toMetadata (AttributeEntry _ _) = mempty
  toMetadata (Comment _) = mempty

-- | A Block consists, syntactically, of one or more contiguous and complete
-- lines of text. Some block types can contain other blocks.
data Block a
  = -- | Regular paragraph.
    Paragraph [BlockPrefixItem UnparsedInline] a
  | -- | This data constructor is not used during parsing, it requires an
    -- additional "nesting" pass.
    --
    -- There can be a @Section@ inside an, e.g., open block, but it needs to
    -- have style @discrete@.
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
    Nestable NestableBlockType [BlockPrefixItem UnparsedInline] [Block a]
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

-- INVARIANT: The first element is always a TextLine. This guarantees that an
-- UnparsedInline can always be converted to an Inline.
--
-- TODO: Document how this invariant is preserved.
type UnparsedInline = NonEmpty UnparsedLine

data UnparsedLine
  = TextLine Text
  | CommentLine Text
  deriving (Eq, Show)

-- | Custom parser state for the parser for 'Block's.
data State = State
  { -- | A stack of open 'Nestable' blocks.
    -- Innermost element is the top of the stack.
    --
    -- For every nestable block we store:
    --
    -- * The syntactic 'Delimiter' used to open the block.
    --   This is what we need to recognize the matching closing delimiter.
    -- * A stack (represented with a list) of list item markers previously used
    --   in the current (possibly nested, aka multi-level, list).
    --   If the parser position is not currently on a list, the stack is empty.
    --
    -- The list representing the stack of open nestable blocks is non-empty: at
    -- the bottom of the stack there is always a value representing the
    -- top-level document (defined in 'State's @Monoid@ instance), so a
    -- one-element stack indicates no nestable block has been open.
    openBlocks :: NonEmpty (Marker DelimiterChar, [Marker ListChar]),
    -- | An environment mapping attribute names to their values (i.e. inlines).
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
      { -- We use @'*' :* 0@ as an arbitrary value that is always present as the
        -- bottom of the stack.
        openBlocks = (AsteriskD :* 0, []) :| [],
        env = mempty
      }

type Parser m = Parsec.ParsecT [Text] State m

pDocument :: Monad m => Parser m [Block UnparsedInline]
pDocument = option () pInclude *> pInitialBlankLines *> pBlocks

pBlocks :: Monad m => Parser m [Block UnparsedInline]
pBlocks = many (pBlock []) <?> "blocks"

pBlock :: Monad m => [LP.LineParser Text] -> Parser m (Block UnparsedInline)
pBlock extraParagraphFinalizers = do
  prefix <- option [] (NE.toList <$> pBlockPrefix)
  pBlock' prefix <?> "block"
  where
    pBlock' prefix =
      (pNestable prefix <?> "nestable")
        <|> (pSectionHeader prefix <?> "section header")
        <|> (pList prefix <?> "list")
        <|> (pParagraph prefix extraParagraphFinalizers <?> "paragraph")
        <|> (pDanglingBlockPrefix prefix <?> "dangling block prefix")

pBlockPrefix :: Monad m => Parser m (NonEmpty (BlockPrefixItem UnparsedInline))
pBlockPrefix = some pBlockPrefixItem <?> "block prefix"
  where
    pBlockPrefixItem =
      Comment <$> pBlockComment
        <|> Comment <$> pLineCommentSequence
        <|> pAttributeEntry
        <|> pBlockId
        <|> pBlockAttributeList
        <|> pBlockTitle

pBlockComment :: Monad m => Parser m Comment
pBlockComment = do
  _ :* n <- choice $ fmap pLine' $ LP.runOfN 4 [SlashC]
  -- We use here an alternative version of pLine, called pLine', that does not
  -- try to handle pre-processor directives, as includes have no effect inside
  -- block comments.
  ts <-
    manyTill (pLine' LP.anyRemainder) $
      eitherP (pLine' (LP.count n SlashC)) Parsec.eof
  option () pInclude
  _ <- many pBlankLine
  pure $ BlockComment ts
{-# ANN pBlockComment ("HLint: ignore" :: String) #-}

pLineCommentSequence :: Monad m => Parser m Comment
pLineCommentSequence =
  LineCommentSequence <$> some pLineComment <* many pBlankLine

-- | Parses a line starting with *exactly* two '/'s.
pLineComment :: Monad m => Parser m Text
pLineComment =
  pLine (LP.string "//" *> Parsec.notFollowedBy (char '/') *> LP.anyRemainder)

-- TODO. Add attribute continuations.
pAttributeEntry :: Monad m => Parser m (BlockPrefixItem a)
pAttributeEntry = pAttributeEntry' <* many pBlankLine
  where
    pAttributeEntry' = do
      (k, v) <-
        pLine
          ( (,) <$ LP.char ':' <*> LP.some alphaNum
              <* LP.char ':'
              <* LP.some space <*> LP.anyRemainder
          )
      -- TODO. Replace to a general parseInline with a SubstitutionGroup
      -- parameter.
      let v' = parseInline' v
      Parsec.modifyState $ \st -> st {env = Map.insert k v' (env st)}
      pure $ AttributeEntry k $ Just (parseInline' v)

pBlockId :: Monad m => Parser m (BlockPrefixItem a)
pBlockId = pBlockId' <* many pBlankLine
  where
    pBlockId' = MetadataItem . BlockId <$> pLine LP.blockId

pBlockAttributeList :: Monad m => Parser m (BlockPrefixItem a)
pBlockAttributeList = pBlockAttributeList' <* many pBlankLine
  where
    pBlockAttributeList' =
      MetadataItem . BlockAttributeList
        <$> pLine LP.blockAttributeList

pBlockTitle :: Monad m => Parser m (BlockPrefixItem UnparsedInline)
pBlockTitle = pBlockTitle' <* many pBlankLine
  where
    pBlockTitle' =
      MetadataItem . BlockTitle . (:| []) . TextLine
        <$> pLine (LP.char '.' *> (LP.satisfy (not . isSpace) <> LP.anyRemainder))

-- | Parses a nestable delimited block.
pNestable ::
  Monad m =>
  [BlockPrefixItem UnparsedInline] ->
  Parser m (Block UnparsedInline)
pNestable prefix = do
  c <- pOpenDelimiter [AsteriskD, EqualsSignD]
  bs <- manyTill (pBlock []) $ eitherP pCloseDelimiter Parsec.eof
  _ <- many pBlankLine
  pure $ case c of
    AsteriskD -> Nestable Sidebar prefix bs
    HyphenD -> error "pNestable: HyphenD case not implemented yet"
    EqualsSignD -> Nestable Example prefix bs

-- | Parses a section header and computes its level.
--
-- __POST-CONDITION__: The computed level is greater or equal to 0.
pSectionHeader ::
  Monad m =>
  [BlockPrefixItem UnparsedInline] ->
  Parser m (Block UnparsedInline)
pSectionHeader prefix = do
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
      header <- pSectionHeader'
      _ <- many pBlankLine
      pure $ SectionHeaderBlock prefix header
  where
    pSectionHeader' =
      (\(_c :* n, value) -> SectionHeader (TextLine value :| []) (n - 1))
        <$> pLine
          ( (,)
              <$> choice (LP.runOfN 1 [EqualsSignH]) <* some space
                <*> (LP.satisfy (not . isSpace) <> LP.anyRemainder)
          )
    style = metadataStyle $ toMetadata $ fmap (fmap parseInline'') prefix

pList ::
  (Monad m) =>
  [BlockPrefixItem UnparsedInline] ->
  Parser m (Block UnparsedInline)
pList prefix =
  pList' prefix <* many pBlankLine
  where
    allUnorderedMarkers = LP.runOfN 1 [AsteriskL, HyphenL]
    pList' prefix' = do
      state <- Parsec.getState
      let allowedMarkers = allUnorderedMarkers
          -- Disallow as markers those markers already in use in the current
          -- list tree of the innermost open block
          disallowedMarkers = snd currentBlock
          (currentBlock, otherBlocks) = NE.head &&& NE.tail $ openBlocks state
      -- Accept item with a new marker
      (marker@(c :* n), firstLine) <-
        pItemFirstLine allowedMarkers disallowedMarkers
      -- Add new marker to the state
      Parsec.setState $
        state
          { openBlocks =
              (fst currentBlock, marker : disallowedMarkers) :| otherBlocks
          }
      -- Complete the first item, using the already parsed first line
      firstItem <-
        pItem firstLine
          <?> "first item " <> T.unpack (fromMarker marker)
      -- Accept items with the same marker of the first item
      nextItems <-
        many
          ( pItemFirstLine [LP.count n c] []
              >>= pItem . snd <?> "item " <> T.unpack (fromMarker marker)
          )
      -- Recover state present at the beginning of the function. Functions like
      -- pItem could have modified it.
      Parsec.setState state
      pure $ List (Unordered Nothing) prefix' (firstItem :| nextItems)
    pItemFirstLine =
      \x -> pLine . itemFirstLine x
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
    pItem firstLine = do
      -- As we are inside a list, any list marker is a finalizer of the current
      -- item (no blank line needed)
      nextLines <-
        many $
          pParagraphContinuation [snd <$> itemFirstLine allUnorderedMarkers []]
      nextBlocks <-
        option
          []
          ( ((: []) <$> pSublist <?> "sublist")
              <|> catMaybes <$> many (pListContinuation <?> "list continuation")
              <?> "next blocks"
          )
      _ <- many pBlankLine
      pure $ Paragraph [] (TextLine firstLine :| nextLines) :| nextBlocks
    -- __Divergence DVB001 from Asciidoctor__. Before sublist:
    --
    --     * Full prefix (including attributes and block title) is allowed.
    --
    --     * Any number of blank lines is allowed.
    --
    -- Probably a linter should warn against any block prefix not preceded by
    -- blank lines.
    pSublist = Parsec.try $ do
      _ <- many pBlankLine
      prefix' <- option [] (NE.toList <$> pBlockPrefix)
      pList prefix'
    -- __Divergence DVB002 from Asciidoctor__: As in classic AsciiDoc, no blank
    -- lines are allowed before the @+@ sign.
    pListContinuation :: Monad m => Parser m (Maybe (Block UnparsedInline))
    pListContinuation =
      pLine (LP.char '+')
        *> optional pBlankLine
        *> optional (pBlock [snd <$> itemFirstLine allUnorderedMarkers []])

pParagraph ::
  Monad m =>
  [BlockPrefixItem UnparsedInline] ->
  [LP.LineParser Text] ->
  Parser m (Block UnparsedInline)
pParagraph prefix extraFinalizers =
  Paragraph prefix <$> pParagraph' <* many pBlankLine
  where
    pParagraph' =
      (:|) <$> pFirst <*> many (pParagraphContinuation extraFinalizers <?> "paragraph continuation")
    pFirst :: Monad m => Parser m UnparsedLine
    pFirst =
      TextLine
        <$> pLineNoneOf
          -- Nestable
          ( (fmap fromMarker <$> LP.runOfN 4 [AsteriskD, EqualsSignD])
              <> [
                   -- Blank line
                   pure ""
                 ]
          )

-- Line comments (but not block comments!) can be contained in a paragraph.
pParagraphContinuation :: Monad m => [LP.LineParser Text] -> Parser m UnparsedLine
pParagraphContinuation extraFinalizers =
  CommentLine <$> pLineComment
    <|> TextLine
      <$> pLineNoneOf
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

pDanglingBlockPrefix ::
  Monad m =>
  [BlockPrefixItem UnparsedInline] ->
  Parser m (Block UnparsedInline)
pDanglingBlockPrefix [] = empty
pDanglingBlockPrefix prefix =
  DanglingBlockPrefix prefix
    <$ Parsec.lookAhead (pCloseDelimiter <|> Parsec.eof)

pInitialBlankLines :: Monad m => Parser m [Text]
pInitialBlankLines = many pBlankLine

pBlankLine :: Monad m => Parser m Text
pBlankLine = pLine $ pure ""

-- | Argument can be a parser for the beginning of the line. Function checks
-- that the part of the line not parsed is whitespace.
--
-- If the line is parsed successfully, this combinator checks if an include line
-- follows. If that is the case it inserts the corresponding lines into the
-- input stream of the parser.
pLine :: Monad m => LP.LineParser a -> Parser m a
pLine p = do
  result <- pLine' p
  option () pInclude
  pure result

-- | A version of 'pLine' that does not check if the line is followed by an
-- include.
pLine' :: Monad m => LP.LineParser a -> Parser m a
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
pLineOneOf :: Monad m => [LP.LineParser a] -> Parser m a
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
pLineNoneOf :: Monad m => [LP.LineParser a] -> Parser m Text
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

pInclude :: Parser m ()
pInclude = empty

-- pInclude = do
--   (filename, arguments) <-
--     pLine' $
--       (,)
--         <$ LP.string "include::"
--         <*> LP.many (satisfy (/= '[')) <* char '['
--         <*> LP.many (satisfy (/= ']')) <* char ']'
--   current <- Parsec.getInput
--   -- TODO. Read actual file content, this is a stub.
--   Parsec.setInput $ ["// (STUB) include::" <> filename <> "[" <> arguments <> "]"] <> current
--   -- Recursive call to handle the case in which the first line of the included
--   -- file is also an include.
--   option () pInclude

pOpenDelimiter ::
  Monad m =>
  [SpecialChar DelimiterChar] ->
  Parser m (SpecialChar DelimiterChar)
pOpenDelimiter cs = do
  -- Parsec.lookAhead needed here because in case we fail later on (because the
  -- block is already open) we don't want to consume any input.
  (c :* n) <- Parsec.lookAhead $ Parsec.try $ pLineOneOf (LP.runOfN 4 cs)
  st <- Parsec.getState
  -- If block is already open (the delimiter is in the stack of open blocks),
  -- we're not opening it again, but fail. In case we don't fail, we consume the
  -- line that was looked ahead above.
  if (c :* n) `elem` (fst <$> openBlocks st)
    then empty
    else
      ( do
          Parsec.putState (st {openBlocks = (c :* n, []) <| openBlocks st})
          -- Consume one token (aka one line of input), and following blanklines
          _ <- pLine LP.anyRemainder
          _ <- many pBlankLine
          -- satisfyToken (const $ Just ())
          pure c
      )

pCloseDelimiter :: Monad m => Parser m ()
pCloseDelimiter = do
  st <- Parsec.getState
  let (c :* n, _) = NE.head (openBlocks st)
  case NE.tail (openBlocks st) of
    -- In presence of DanglingBlockPrefix'es, we can try to pop from an
    -- openBlocks stack that contains the initial open block only. We do nothing
    -- in this case.
    [] -> pure ()
    b : bs -> do
      -- If (n, c) found in openBlocks stack, pop one element. Only consume line
      -- from input (and look for includes) if the found delimiter matches
      -- openBlocks' top.
      _ <-
        pLine (LP.count n c)
          <|> Parsec.lookAhead
            ( choice $
                fmap (\(c' :* n', _) -> pLine' (LP.count n' c')) (b : bs)
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

-- | TODO. Stub until proper inline parsing is implemented.
parseInline'' :: UnparsedInline -> Inline
parseInline'' (TextLine first :| following) =
  InlineSeq $ Word first :| mapMaybe parse following
  where
    parse (TextLine t) = Just $ Word t
    parse (CommentLine _) = Nothing
-- See INVARIANT.
parseInline'' _ = error "parseInline'': First element should be a TextLine"
