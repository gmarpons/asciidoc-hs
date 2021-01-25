{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Text.AsciiDoc.SpecialChars
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines an algebraic type for describing characters that have a
-- special meaning in AsciiDoc, like @*@, @=@, or @/@.
--
-- Data constructors of the 'SpecialChar' type (like 'AsteriskD' or 'HyphenL')
-- can be used to preserve full information about what has been parsed.
-- This is useful, for example, to apply the "parse, don't validate" principle.
-- Suffixes "D" or "L" in the aforementioned constructors are explained in the
-- documentation of type 'SpecialChar'.
module Text.AsciiDoc.SpecialChars
  ( -- * Special characters
    CommentChar,
    DelimiterChar,
    HeaderChar,
    ListChar,
    SpecialChar (..),
    fromSpecialChar,

    -- * Marker type
    Marker (..),
    fromMarker,
  )
where

import Data.Text (Text)
import qualified Data.Text as T (justifyRight)

-- | Used as a type parameter in 'SpecialChar' to indicate that a character is
-- used in a delimiter for a block comment or in a marker for a line comment.
data CommentChar deriving (Eq, Show)

-- | Used as a type parameter in 'SpecialChar' to indicate that a character is
-- used in a block delimiter.
data DelimiterChar deriving (Eq, Show)

-- | Used as a type parameter in 'SpecialChar' to indicate that a character is
-- used in a section header marker.
data HeaderChar deriving (Eq, Show)

-- | Used as a type parameter in 'SpecialChar' to indicate that a character is
-- used in a list item marker.
data ListChar deriving (Eq, Show)

-- | Algebraic type for describing characters that have a special meaning in
-- AsciiDoc, like @*@, @=@, or @/@.
--
-- Every constructor defines a tuple @(character, function in AsciiDoc)@.
-- The last letter of every constructor name indicates the function.
-- The same character can be used in more than one function.
-- We use GADTs to represent those tuples, as in this way we get more precise
-- type checking with no code duplication.
-- The list of possible functions is:
-- * 'CommentChar'
-- * 'DelimiterChar'
-- * 'HeaderChar'
-- * 'ListChare'
data SpecialChar a where
  SlashC :: SpecialChar CommentChar
  AsteriskD :: SpecialChar DelimiterChar
  HyphenD :: SpecialChar DelimiterChar
  EqualsSignD :: SpecialChar DelimiterChar
  EqualsSignH :: SpecialChar HeaderChar
  AsteriskL :: SpecialChar ListChar
  HyphenL :: SpecialChar ListChar

deriving instance (Eq a) => Eq (SpecialChar a)

deriving instance (Show a) => Show (SpecialChar a)

fromSpecialChar :: SpecialChar a -> Char
fromSpecialChar = \case
  SlashC -> '/'
  AsteriskD -> '*'
  HyphenD -> '-'
  EqualsSignD -> '='
  EqualsSignH -> '='
  AsteriskL -> '*'
  HyphenL -> '-'

-- | An algebraic data type for describing a sequence of special characters that
-- can be:
-- * A marker that signals the start of a new element in the document.
--   Examples of markers are:
--     * "@*@" for starting a new list item.
--     * "@===@" for starting a new level 2 section header.
-- * A delimiter.
--   Examples of delimiters are:
--     * "@****@" or "@====@" or other openings and closings of delimited
--       blocks.
data Marker a
  = -- | Most markers consist in a single character repeated a number of times.
    SpecialChar a :* Int

deriving instance Eq (Marker CommentChar)

deriving instance Eq (Marker DelimiterChar)

deriving instance Eq (Marker ListChar)

deriving instance Show (Marker CommentChar)

deriving instance Show (Marker DelimiterChar)

deriving instance Show (Marker ListChar)

fromMarker :: Marker a -> Text
fromMarker (c :* n) = T.justifyRight n (fromSpecialChar c) ""
