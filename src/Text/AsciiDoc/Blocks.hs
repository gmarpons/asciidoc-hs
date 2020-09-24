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
    Block (..),

    -- ** BlockPrefixElement
    BlockPrefixItem (..),

    -- * Parsers
    Parser,
    pBlocks,
    pBlock,
    pBlockPrefixItem,
    pParagraph,
    pNullBlock,

    -- * Low-level parsers
    satisfyToken,
    anyToken,
    pUnparsedLine,
    pInclude,
    pEndOfInclude,
    pAttributeEntry,

    -- * Testing
    parseTest,
    parseFile,
    readTokens,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (eitherP, many, optional)
import Control.Monad.Combinators.NonEmpty
import Data.Char (isSpace)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.AsciiDoc.Inlines hiding (parseTest)
import Text.AsciiDoc.Preprocessor
import Text.AsciiDoc.Token
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (SourcePos)

type HeaderLevel = Int

-- | An explicit header level is necessary, as the output style (e.g. font size)
-- depends on the actual number of @=@'s found.
--
-- The number of @=@'s when @Wrapper@s are implemented.
data SectionHeader = SectionHeader Inline HeaderLevel
  deriving (Eq, Show)

data ListCheckStatus
  = Checked
  | Unchecked
  deriving (Eq, Show)

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

-- | The @Int@ is the indentation of the block. If the @Literal@ block is not
-- signaled by indentation (i.e., @....@ or @[literal]@ is used), then
-- indentation is 0 (all preceding space is copied verbatim as content).
newtype LiteralIndentation = LiteralIndentation Int
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

data BlockMacroType
  = ImageBlockMacro
  | TableOfContentsMacro
  | CustomBlockMacro
  deriving (Eq, Show)

data MacroArguments = MacroArguments
  deriving (Eq, Show)

-- | Mainly semantic info, but also takes into account the different syntactic
-- variations to be able to reproduce the source text.
data Block a
  = -- | Regular paragraph.
    Paragraph a Inline
  | -- | There can be a @Section@ inside an, e.g., open block, but it needs to
    -- have the "discrete" attribute. We consider that the @BlockMetadata@
    -- belongs to the whole section, and not the section header, as this is more
    -- flexible.
    --
    -- INVARIANT: @HeaderLevel@ in 'SectionHeader' is consistent with nesting.
    --
    -- TODO. See if Asciidoctor assigns roles to sections (div) or to headers.
    -- At least the @[abstract]@ name is applied to the whole section.
    Section a SectionHeader [Block a]
  | List a ListType (NonEmpty (NonEmpty (Block a)))
  | Table a {- TODO. Many things here -}
  | ThematicBreak a
  | PageBreak a
  | -- | Block with no real contents, except for comments. It's used to store
    -- block properties at the end of the file (that have potential semantic
    -- relevance if the file is used as an include).
    NullBlock a
  | -- | Sequence of blocks of some defined type that allows nested blocks
    -- inside (i.e. admonition, sidebar, example, quote, and open block with no
    -- other standard type).
    Nestable a NestableBlockType [Block a]
  | VerseBlock a [Inline]
  | -- | Block type determines substitution group applied: @Verbatim@ or @None@
    -- (aka passthrough).
    --
    -- TODO: Check that designed pipeline guarantees that pre-processor
    -- directives are expanded (if not escaped) even in literal blocks, as
    -- https://asciidoctor.org/docs/user-manual/#include-processing states.
    LiteralBlock a LiteralBlockType [Text]
  | -- | Some macros accept block metadata, as e.g. @toc::[]@, that accepts
    -- defining its title with @.TITLE@ syntax.
    BlockMacro a BlockMacroType MacroArguments
  | IncludeStart FilePath IncludeOptions
  | IncludeEnd
  deriving (Eq, Show, Functor)

-- | A Block can be preceded by an arbitrary (finite) list of
-- @BlockAttributes@s.
--
-- This is a semi-syntactic element. Every value of this type comes from a
-- source line.
data BlockPrefixItem
  = -- | A block can have more than one ID (aka anchor), and all of them can be
    -- used in cross-references.
    BlockId Text
  | -- | A block can be preceded by any number of @BlockTitle@s (aka labels). Only
    -- the last one is semantically relevant.
    BlockTitle Inline
  | -- | A block can be preceded by any number of @BlockAttributeList@s. For
    -- positional arguments, only the last list is taken into account.
    --
    -- Some of the elements of the list can be name-value pairs, or
    --
    -- TODO. See if elements of different lists are combined in some cases.
    --
    -- TODO. Check if some attributes in the list can contain full inlines, as
    -- it's the case with standalone (aka attribute entry) attributes.
    BlockAttributeList [Text]
  | -- | Attributes can contain formatting, see
    -- https://asciidoctor.org/docs/user-manual/#attribute-limitations.
    --
    -- TODO. Multi-line attribute definitions, see
    -- https://asciidoctor.org/docs/user-manual/#splitting-attribute-values-over-multiple-lines.
    AttributeDefinition AttributeId Inline
  | AttributeUnsetting AttributeId
  | CommentBlockDelimiter
  | -- | Comments are considered part of the contents, and not stripped out if not
    -- explicitly requested for in the command-line.
    Comment Text
  | BlankLine
  deriving (Eq, Show)

type Parser = Parsec.Parsec [Token Text] ()

satisfyToken :: (Token Text -> Maybe a) -> Parser a
satisfyToken matcher = Parsec.tokenPrim show updatePos matcher
  where
    updatePos :: SourcePos -> Token Text -> [Token Text] -> SourcePos
    updatePos pos _ _ = Parsec.incSourceLine pos 1

anyToken :: Parser (Token Text)
anyToken = Parsec.tokenPrim show updatePos matcher
  where
    matcher t = Just t
    updatePos :: SourcePos -> Token Text -> [Token Text] -> SourcePos
    updatePos pos _ _ = Parsec.incSourceLine pos 1

pUnparsedLine :: Parser Text
pUnparsedLine = satisfyToken f
  where
    f (UnparsedLine t) = Just t
    f _ = Nothing

pInclude :: Parser (Block [BlockPrefixItem])
pInclude = satisfyToken f
  where
    f (Include file options) = Just (IncludeStart file options)
    f _ = Nothing

pEndOfInclude :: Parser (Block [BlockPrefixItem])
pEndOfInclude = satisfyToken f
  where
    f EndOfInclude = Just IncludeEnd
    f _ = Nothing

pAttributeEntry :: Parser (BlockPrefixItem)
pAttributeEntry = satisfyToken f
  where
    f (AttributeEntry k v) = Just (AttributeDefinition k v)
    f _ = Nothing

pBlocks :: Parser [Block [BlockPrefixItem]]
pBlocks =
  f <$> many pBlock <*> optional pNullBlock <* Parsec.eof
  where
    f bs (Just b) = bs <> [b]
    f bs Nothing = bs

pBlock :: Parser (Block [BlockPrefixItem])
pBlock =
  f <$> many pBlockPrefixItem <*> pBlock'
  where
    pBlock' =
      pPageBreak <|> pParagraph
    f blockPrefix block = fmap (const blockPrefix) block

pBlockPrefixItem :: Parser BlockPrefixItem
pBlockPrefixItem =
  pBlankLine

pNullBlock :: Parser (Block [BlockPrefixItem])
pNullBlock = NullBlock . NE.toList <$> some pBlockPrefixItem

pParagraph :: Parser (Block [BlockPrefixItem])
pParagraph =
  f <$> some pParagraphLine <* (eitherP pBlankLine Parsec.eof)
  where
    -- TODO. Intercalate real newlines from source file.
    f ts = Paragraph [] $ parseInline $ T.concat $ intersperse "\n" $ NE.toList ts

pBlankLine :: Parser BlockPrefixItem
pBlankLine = satisfyToken f
  where
    f (UnparsedLine t) | T.all isSpace t = Just BlankLine
    f _ = Nothing

pParagraphLine :: Parser Text
pParagraphLine = satisfyToken f
  where
    -- TODO. Also check no indentation.
    f (UnparsedLine t) | T.any (not . isSpace) t = Just t
    f _ = Nothing

-- XXX: Does not match whitespace after <<<
pPageBreak :: Parser (Block [BlockPrefixItem])
pPageBreak = PageBreak [] <$ satisfyToken f
  where
    f (UnparsedLine t) | t == "<<<" = Just ()
    f _ = Nothing

parseTest :: Parser a -> [Token Text] -> Either Parsec.ParseError a
parseTest parser tokens =
  Parsec.runParser parser () "" tokens

readTokens :: FilePath -> IO [Token Text]
readTokens file = do
  t <- T.readFile file
  tokenize t

parseFile :: FilePath -> IO (Either Parsec.ParseError [Block [BlockPrefixItem]])
parseFile file = do
  tokens <- readTokens file
  pure $ parseTest pBlocks tokens
