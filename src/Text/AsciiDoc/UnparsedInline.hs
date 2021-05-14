-- |
-- Module      :  Text.AsciiDoc.UnparsedInline
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for working with lines meant to be parsed together into an
-- 'Text.AsciiDoc.Inline'.
module Text.AsciiDoc.UnparsedInline
  ( UnparsedInline,
    UnparsedLine (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- INVARIANT: The first element is always a TextLine. This guarantees that an
-- UnparsedInline can always be converted to an Inline.
--
-- TODO: Document how this invariant is preserved.
type UnparsedInline = NonEmpty UnparsedLine

data UnparsedLine
  = TextLine Text
  | CommentLine Text
  deriving (Eq, Show)
