{-# LANGUAGE LambdaCase #-}

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
  ( convertDocument,
  )
where

import qualified Data.Text as T
import Text.AsciiDoc.Inlines
import qualified Text.Pandoc.Builder as Pandoc
import Text.Pandoc.Definition (Pandoc)

type Document = [Inline]

convertDocument :: Document -> Pandoc
convertDocument is =
  Pandoc.setTitle "Testing Document Title"
    $ Pandoc.doc
    $ Pandoc.para
    $ foldMap convertInline is

convertInline :: Inline -> Pandoc.Inlines
convertInline = \case
  Space -> Pandoc.space
  Word text -> Pandoc.str text
  Bold inlines -> Pandoc.strong $ foldMap convertInline inlines
  Italic inlines -> Pandoc.emph $ foldMap convertInline inlines
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
  Monospace inlines ->
    let attributes = ("", ["monospace"], [])
     in Pandoc.spanWith attributes $ foldMap convertInline inlines
  Symbol char -> Pandoc.str $ T.singleton char
  Fallback text -> Pandoc.str text
