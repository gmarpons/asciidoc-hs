{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
    convertBlock,
    convertInline,
    -- TODO. Relocate parseInlines function
    parseInlines,
  )
where

import Data.List.NonEmpty as NE (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last (Last))
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.Blocks hiding (Parser, State)
import Text.AsciiDoc.Inlines hiding (Parser, State)
import Text.AsciiDoc.Metadata
import Text.AsciiDoc.UnparsedInline
import Text.Pandoc.Builder (Attr)
import qualified Text.Pandoc.Builder as Pandoc
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Parsec as Parsec

-- type Document = [Block Inline]

parseInlines :: Document UnparsedInline -> Document Inline
parseInlines = fmap parseInline

parseInline :: UnparsedInline -> Inline
parseInline x =
  case Parsec.runParser inlinesP inlineParserInitialState "" (fromUnparsedInline x) of
    Right result -> result
    Left parseError -> error $ "parseInlines: " <> show parseError
  where
    fromUnparsedInline :: UnparsedInline -> Text
    -- TODO. Check what line finalizers T.unilines considers, and update the
    -- following definition to be congruent with Text.AsciiDoc.Inlines.newlineP.
    fromUnparsedInline = T.unlines . fmap toText . NE.toList
    toText :: InputLine -> Text
    toText = \case
      MarkupLine t -> t
      CommentLine _ -> ""

convertDocument :: Document Inline -> Pandoc
convertDocument (Document mHeader bs) =
  Pandoc.setMeta "pagetitle" pageTitle $
    Pandoc.doc $
      -- Divergence from Asciidoctor: the following div with role "article" is
      -- not necessary in Asciidoctor because "article" role (and other metadata
      -- in @metadata@) is assigned to the <body> tag.
      Pandoc.divWith
        (toAttr $ articleMetadata {metadataRoles = "article" : metadataRoles articleMetadata})
        ( Pandoc.divWith
            (toAttr $ mempty {metadataIds = ["header"]})
            -- Spurious @plain@ block needed to avoid Pandoc fuse <div> and <h*>
            (Pandoc.plain " " <> headerContent)
            <> Pandoc.divWith
              (toAttr $ mempty {metadataIds = ["content"]})
              -- Spurious @plain@ block needed to avoid Pandoc fuse <div> and <h*>
              (Pandoc.plain " " <> contentTitle <> foldMap convertBlock bs)
        )
  where
    -- TODO. Var sectionMetadata will be used when correct section nesting is
    -- computed
    (pageTitle, articleMetadata, _sectionMetadata, headerContent, contentTitle) =
      case mHeader of
        Just (DocumentHeader p level i) ->
          let m = toMetadata p :: Metadata UnparsedInline
              t = convertInline i
              h = Pandoc.header (level + 1) t
           in if level == 0
                then
                  ( t,
                    m,
                    mempty :: Metadata UnparsedInline,
                    h,
                    mempty
                  )
                else
                  ( t,
                    m {metadataIds = [], metadataRoles = []},
                    mempty {metadataRoles = metadataRoles m},
                    mempty,
                    Pandoc.divWith (toAttr $ mempty {metadataIds = metadataIds m}) h
                  )
        Nothing -> ("Untitled", mempty, mempty, mempty, mempty)

convertBlock :: Block Inline -> Pandoc.Blocks
convertBlock = \case
  Paragraph p i ->
    -- We cannot use <> to extend m because, to mimic Asciidoctor, metadataRoles
    -- appending doesn't preserve the left value if the right one uses
    -- "role=..." syntax. See Monoid Metadata instance.
    let m = toMetadata p
     in Pandoc.divWith (toAttr $ m {metadataRoles = "paragraph" : metadataRoles m}) $
          prependTitleDiv (fmap parseInline m) $
            Pandoc.para $ convertInline i
  -- Divergence from Asciidoctor: we add the possible title in the section
  -- prefix to the header of the section, and not to the first non-header block
  -- found, as Asciidoctor does.
  Section p level i bs ->
    let m = toMetadata p
        mSect = m {metadataRoles = T.pack ("sect" ++ show (level + 1)) : metadataRoles m}
     in Pandoc.divWith (toAttr mSect) $
          prependTitleDiv (fmap parseInline m) $
            Pandoc.headerWith mempty (level + 1) (convertInline i)
              <> foldMap convertBlock bs
  -- TODO. Compute Section's (nesting) before converting. The following case
  -- should be redundant.
  -- TODO. Add a Metadata value to SectionHeaderBlock and avoid recalculating it
  SectionHeader p level i ->
    let m = toMetadata p
     in Pandoc.divWith (toAttr m) $
          prependTitleDiv (fmap parseInline m) $
            Pandoc.headerWith mempty (level + 1) (convertInline i)
  List (Unordered Nothing) p bss ->
    let m = toMetadata p
     in Pandoc.divWith (toAttr $ m {metadataRoles = "ulist" : metadataRoles m}) $
          prependTitleDiv (fmap parseInline m) $
            Pandoc.bulletList $ NE.toList $ fmap (foldMap convertBlock) bss
  -- TODO. Cover other list types
  List _ _p _ -> undefined
  Nestable Example p bs ->
    let m = toMetadata p
        -- TODO. This prefix should come from a (localized) variable (L10N)
        prefix = parseInline $ MarkupLine "Example 1. " :| []
        title = fmap ((prefix <>) . parseInline) <$> metadataTitle m
     in Pandoc.divWith (toAttr $ m {metadataRoles = "exampleblock" : metadataRoles m}) $
          prependTitleDiv (m {metadataTitle = title}) $
            Pandoc.divWith (toAttr $ mempty {metadataRoles = ["content"]}) $
              foldMap convertBlock bs
  Nestable Sidebar p bs ->
    let m = toMetadata p
     in Pandoc.divWith (toAttr $ m {metadataRoles = "sidebarblock" : metadataRoles m}) $
          Pandoc.divWith (toAttr $ mempty {metadataRoles = ["content"]}) $
            -- In contrast with Nestable Example, here the title is part of the
            -- content, to mimic Asciidoctor
            prependTitleDiv (fmap parseInline m) $
              foldMap convertBlock bs
  -- TODO. Cover other nestable types
  Nestable _ _p _ -> undefined
  DanglingBlockPrefix _ -> mempty

convertInline :: Inline -> Pandoc.Inlines
convertInline = \case
  AlphaNum t -> Pandoc.str t
  EndOfInline _ -> mempty
  Newline t -> Pandoc.str t
  Space _ -> Pandoc.space
  InlineSeq inlines -> foldMap convertInline inlines
  StyledText Bold as _ inlines _
    | as == defaultAttributeList ->
      Pandoc.strong $ foldMap convertInline inlines
  -- NOTE. Asciidoctor creates a single <strong> element with attributes in this
  -- case, but Pandoc AST doesn't support attributes in <strong> nodes. The
  -- following is semantically equivalent:
  StyledText Bold as _ inlines _ ->
    Pandoc.spanWith (toAttr $ toMetadata as) $
      Pandoc.strong $ foldMap convertInline inlines
  -- NOTE. Pandoc's AST don't support "mark" elements, but the HTML writer
  -- produces a <mark> element if the class "mark" is added to the span.
  --
  -- As Asciidoctor, we only produce a <mark> element for "empty" custom spans.
  StyledText Custom as _ inlines _ ->
    let m = toMetadata as :: Metadata UnparsedInline
        -- Returns a functions that *can* enclose its argument into a span,
        -- depending on 'as'.
        encloseInSpan :: Pandoc.Inlines -> Pandoc.Inlines
        encloseInSpan
          | as == defaultAttributeList =
            Pandoc.spanWith (toAttr $ mempty {metadataRoles = ["mark"]})
          -- If attributes are not significant for translation to Pandoc, in
          -- congruence with Asciidoctor we skip the superfluous span.
          | toAttr m == mempty = id
          | otherwise = Pandoc.spanWith $ toAttr $ toMetadata as
     in encloseInSpan $ foldMap convertInline inlines
  StyledText Italic as _ inlines _
    | as == defaultAttributeList ->
      Pandoc.emph $ foldMap convertInline inlines
  -- NOTE. Asciidoctor creates a single <emph> element with attributes in this
  -- case, but Pandoc AST doesn't support attributes in <emph> nodes. The
  -- following is semantically equivalent:
  StyledText Italic as _ inlines _ ->
    Pandoc.spanWith (toAttr $ toMetadata as) $
      Pandoc.emph $ foldMap convertInline inlines
  -- NOTE. Pandoc's Code nodes do not support nested markup. It's a pity because
  -- HTML <code> element does. Among the three following solutions, we choose 1)
  -- because it's simpler than 2) and produces cleaner (probably more robust for
  -- backends that are not HTML) Pandoc output, and with 3) a lot of markup is
  -- potentially lost. Also, it's congruent with the treatment of <mark> above:
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
  --
  -- TODO. Find a solution that can work for other writers than HTML.
  StyledText Monospace as _ inlines _ ->
    Pandoc.spanWith (toAttr $ mempty {metadataRoles = ["monospace"]} <> toMetadata as) $
      foldMap convertInline inlines
  Symbol t -> Pandoc.str t

toAttr :: Metadata UnparsedInline -> Attr
toAttr m = (identifier, classes, keyvals)
  where
    identifier = case metadataIds m of
      [] -> ""
      (x : _) -> x -- TODO. Support multiple id's per entity.
    classes = metadataRoles m
    -- We could do
    --   Map.toList $ metadataNamedAttributes m
    -- but keyvals is used by Pandoc for data-* attributes.
    keyvals = []

-- | In case the metadata contains a block title, this function prepends a div
-- with the title to the provided 'Pandoc.Blocks' value.
prependTitleDiv :: Metadata Inline -> Pandoc.Blocks -> Pandoc.Blocks
prependTitleDiv m = case metadataTitle m of
  Nothing -> id
  Just (Last i) ->
    mappend $
      Pandoc.divWith (toAttr $ mempty {metadataRoles = ["title"]}) $
        Pandoc.plain $ convertInline i
