{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Text.AsciiDoc.Metadata
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- A flexible Metadata type that can be used to store properties of both
-- 'Text.AsciiDoc.Blocks' and 'Text.AsciiDoc.Inlines'.
--
-- Its 'Semigroup' and 'Monoid' instances codify the non-trivial rules of
-- metadata addition.
module Text.AsciiDoc.Metadata
  ( -- * Metadata Type
    Metadata (..),

    -- * ToMetadata Class
    ToMetadata (..),
  )
where

import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Semigroup (Last (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.Attributes
import Text.AsciiDoc.UnparsedInline

data Metadata a = Metadata
  { metadataStyle :: Maybe (Last Text),
    metadataIds :: [Text],
    metadataRoles :: [Text],
    metadataOptions :: [Text],
    metadataTitle :: Maybe (Last a),
    metadataPositionalAttributes :: IntMap.IntMap Text,
    -- | Named attributes different than @id@, @opts@, @options@, @role@ and
    -- @title@.
    metadataNamedAttributes :: Map.Map Text Text,
    metadataRoleNamedAttribute :: Maybe (Last [Text])
  }
  deriving (Eq, Show)

instance Semigroup (Metadata a) where
  x <> y =
    let a = metadataStyle x <> metadataStyle y
        b = metadataIds x <> metadataIds y
        -- If the right operand contains and explicit "role=ROLENAME" attribute,
        -- it replaces whatever roles contained in the left operand. The right
        -- operand can add new roles with the "[.ROLENAME]" syntax.
        c = case metadataRoleNamedAttribute y of
          Just yRoles -> getLast yRoles
          Nothing -> metadataRoles x <> metadataRoles y
        d = metadataOptions x <> metadataOptions y
        e = metadataTitle x <> metadataTitle y
        -- Semigroup instance from IntMap gives precedence to values from left
        -- operand. In this case we prefer the last value (i.e. value from
        -- operand @y@).
        f = metadataPositionalAttributes y <> metadataPositionalAttributes x
        -- Semigroup instance from Map gives precedence to values from left
        -- operand. In this case we prefer the last value (i.e. value from
        -- operand @y@).
        g = metadataNamedAttributes y <> metadataNamedAttributes x
        h = metadataRoleNamedAttribute x <> metadataRoleNamedAttribute y
     in Metadata
          { metadataStyle = a,
            metadataIds = b,
            metadataRoles = c,
            metadataOptions = d,
            metadataTitle = e,
            metadataPositionalAttributes = f,
            metadataNamedAttributes = g,
            metadataRoleNamedAttribute = h
          }

instance Monoid (Metadata a) where
  mempty =
    Metadata
      { metadataStyle = mempty,
        metadataIds = mempty,
        metadataRoles = mempty,
        metadataOptions = mempty,
        metadataTitle = mempty,
        metadataPositionalAttributes = mempty,
        metadataNamedAttributes = mempty,
        metadataRoleNamedAttribute = mempty
      }

class ToMetadata b a where
  toMetadata :: b -> Metadata a

instance ToMetadata PositionedAttribute UnparsedInline where
  toMetadata (PositionedAttribute (index, PositionalAttribute p)) =
    mempty {metadataPositionalAttributes = IntMap.singleton index p}
  -- Special treatment of attribute names: @id@, @opts@, @options@, @role@ and
  -- @title@.
  toMetadata (PositionedAttribute (_, NamedAttribute "id" v)) =
    mempty {metadataIds = [v]}
  toMetadata (PositionedAttribute (_, NamedAttribute "opts" v)) =
    mempty {metadataOptions = [v]}
  toMetadata (PositionedAttribute (_, NamedAttribute "options" v)) =
    mempty {metadataOptions = [v]}
  toMetadata (PositionedAttribute (_, NamedAttribute "role" v)) =
    mempty
      { metadataRoles = T.words v,
        metadataRoleNamedAttribute = Just $ Last $ T.words v
      }
  toMetadata (PositionedAttribute (_, NamedAttribute "title" v)) =
    mempty {metadataTitle = Just $ Last $ TextLine v :| []}
  -- Any other named attribute
  toMetadata (PositionedAttribute (_, NamedAttribute k v)) =
    mempty {metadataNamedAttributes = Map.singleton k v}
  toMetadata (PositionedAttribute (_, ShorthandSyntaxAttribute s i r o)) =
    mempty
      { metadataStyle = Just (Last s),
        metadataIds = i,
        metadataRoles = r,
        metadataOptions = o
      }

instance
  {-# OVERLAPPABLE #-}
  (Foldable f, ToMetadata b a) =>
  ToMetadata (f b) a
  where
  toMetadata = foldMap toMetadata
