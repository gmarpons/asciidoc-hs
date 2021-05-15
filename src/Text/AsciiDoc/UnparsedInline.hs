{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

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
  ( UnparsedInline (..),
    Line (..),
    WithMarkup,
    WithComment,
    Some (..),
  )
where

import Data.GADT.Compare (GEq (..))
import Data.GADT.Show (GShow, gshowsPrec)
import Data.Some
import Data.Text (Text)
import Data.Type.Equality

-- INVARIANT: The first element is always a TextLine. This guarantees that an
-- UnparsedInline can always be converted to an Inline.
--
-- TODO: Document how this invariant is preserved.

data UnparsedInline = Line WithMarkup :|| [Some Line]
  deriving (Eq, Show)

data Line a where
  LineWithMarkup :: Text -> Line WithMarkup
  LineWithComment :: Text -> Line WithComment

instance GEq Line where
  LineWithMarkup x `geq` LineWithMarkup y = if x == y then Just Refl else Nothing
  LineWithComment x `geq` LineWithComment y = if x == y then Just Refl else Nothing
  _ `geq` _ = Nothing

instance GShow Line where
  gshowsPrec d (LineWithMarkup t) s = showsPrec d t s
  gshowsPrec d (LineWithComment t) s = showsPrec d t s

deriving instance Eq (Line WithMarkup)

deriving instance Eq (Line WithComment)

deriving instance Show (Line WithMarkup)

deriving instance Show (Line WithComment)

data WithMarkup deriving (Eq, Show)

data WithComment deriving (Eq, Show)
