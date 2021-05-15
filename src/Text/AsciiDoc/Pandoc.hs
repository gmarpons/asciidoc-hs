{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  Text.AsciiDoc.Pandoc
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Converters from AsciiDoc AST node types to Pandoc AST node types.
module Text.AsciiDoc.Pandoc
  ( -- * Types
    Document,
    Inline,

    -- * Conversions
    convertDocument,
    convertInline,

    -- * TODO
    parseInlines,
  )
where

import qualified Data.Text as T
import Text.AsciiDoc.Blocks ()
import Text.AsciiDoc.Blocks hiding (Parser)
import Text.AsciiDoc.Inlines
import Text.AsciiDoc.Metadata (ToMetadata (toMetadata))
import Text.AsciiDoc.UnparsedInline
import qualified Text.Pandoc.Builder as Pandoc
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Parsec as Parsec
import Data.Text (Text)
import Data.Some (withSome)

type Document = [Block Inline]

parseInlines :: [Block UnparsedInline] -> Document
parseInlines = (fmap . fmap) parseInline

parseInline :: UnparsedInline -> Inline
parseInline x =
  case Parsec.runParser inlinesP initialState "" (fromUnparsedInline x) of
    Right result -> result
    Left parseError -> error $ "parseInlines: " <> show parseError
  where
    -- TODO. Try to not lose the knowledge about UnparsedInline starting with
    -- some content.
    fromUnparsedInline :: UnparsedInline -> Text
    fromUnparsedInline (LineWithMarkup c :|| ls) =
      T.unlines $ c : fmap toText ls
    toText :: Some Line -> Text
    toText s = withSome s $ \case
        LineWithMarkup t -> t
        LineWithComment t -> t

convertDocument :: Document -> Pandoc
convertDocument bs =
  -- TODO: Document title is a stub
  Pandoc.setTitle "Testing Document Title" $
    Pandoc.doc $
      foldMap convertBlock bs

convertBlock :: Block Inline -> Pandoc.Blocks
convertBlock = \case
  Paragraph _p i -> Pandoc.para $ convertInline i
  Section _p (SectionHeader i n) bs ->
    Pandoc.headerWith ("", [], []) (n + 1) (convertInline i) <> foldMap convertBlock bs
  -- TODO. Add a Metadata value to SectionHeaderBlock and avoid recalculating it
  SectionHeaderBlock p (SectionHeader i n) ->
    flip const (toMetadata @[BlockPrefixItem UnparsedInline] @UnparsedInline p) $ Pandoc.headerWith ("", [], []) (n + 1) (convertInline i)

-- List
-- ThematicBreak
-- PageBreak
-- Nestable
-- VerseBlock ([BlockPrefixItem UnparsedInline]) ([a])
-- LiteralBlock LiteralBlockType ([BlockPrefixItem UnparsedInline]) ([Text])
-- BlockMacro BlockMacroType ([BlockPrefixItem UnparsedInline]) MacroArguments
-- DanglingBlockPrefixP ([BlockPrefixItem UnparsedInline])

convertInline :: Inline -> Pandoc.Inlines
convertInline = \case
  AlphaNum t -> Pandoc.str t
  EndOfInline _ -> mempty
  Newline t -> Pandoc.str t
  Space t -> Pandoc.str t -- Pandoc.space
  InlineSeq inlines -> {- Pandoc.spanWith ("", [], []) $ -} foldMap convertInline inlines
  StyledText Bold (ParameterList parameters) _ inlines _
    | T.null parameters ->
      Pandoc.strong $ foldMap convertInline inlines
  StyledText Bold (ParameterList parameters) _ inlines _ ->
    let attributes = ("", [], [("raw-attributes", parameters)])
     in Pandoc.spanWith attributes $ Pandoc.strong $ foldMap convertInline inlines
  StyledText Custom (ParameterList parameters) _ inlines _ ->
    let attributes = ("", ["custom"], [("raw-attributes", parameters)])
     in Pandoc.spanWith attributes $ foldMap convertInline inlines
  StyledText Italic (ParameterList parameters) _ inlines _
    | T.null parameters ->
      Pandoc.emph $ foldMap convertInline inlines
  StyledText Italic (ParameterList parameters) _ inlines _ ->
    let attributes = ("", [], [("raw-attributes", parameters)])
     in Pandoc.spanWith attributes $ Pandoc.emph $ foldMap convertInline inlines
  -- NOTE. Pandoc's Code nodes do not support nested markup. It's a pity because
  -- HTML <code> element does. Among the three following solutions, we choose 1)
  -- because it's simpler than 2) and produces cleaner (probably more robust for
  -- backends that are not HTML) Pandoc output, and with 3) a lot of markup is
  -- potentially lost:
  --
  -- 1) Convert Monospace into a span, do not call Pandoc.code, and add a
  --    "monospace" class. I.e., do not generate <code> element in HTML and
  --    delegate monospace markup to CSS or another formatting mechanism.
  --
  -- 2) Break inlines into pieces of uniform markup, and apply Pandoc.code as
  --    the inner-most construct of each piece. Apply other markup to the pieces
  --    that need it.
  --
  -- 3) Convert inlines into a Text with no markup applied.
  StyledText Monospace (ParameterList parameters) _ inlines _ ->
    let attributes = ("", ["monospace"], [("raw-attributes", parameters)])
     in Pandoc.spanWith attributes $ foldMap convertInline inlines
  Symbol t -> Pandoc.str t
