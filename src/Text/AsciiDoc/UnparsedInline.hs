{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      :  Text.AsciiDoc.UnparsedInline
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types for working with input lines meant to be parsed together into an
-- 'Text.AsciiDoc.Inline'.
module Text.AsciiDoc.UnparsedInline
  ( UnparsedInline,
    InputLine (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- TODO. Type-encode the invariant of UnparsedInline. See branch
-- safe-unparsed-inline for a POC. Assess if it's worth the extra dependencies
-- (uses package some), or if the knowledge obtained during block parsing can be
-- really preserved for inline parsing (and guarantee that we don't try to
-- parse invalid text).

-- | Non-empty sequence of input lines meant to be parsed together as an inline.
--
-- __Invariant__: The first element is always a 'MarkupLine'.
-- This guarantees that an @UnparsedInline@ can always be converted to an
-- 'Text.AsciiDoc.Inline'.
type UnparsedInline = NonEmpty InputLine

data InputLine
  = MarkupLine Text
  | CommentLine Text
  deriving stock (Eq, Show)
