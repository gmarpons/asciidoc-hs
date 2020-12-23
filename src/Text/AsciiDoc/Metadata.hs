{-# LANGUAGE FlexibleInstances #-}

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
  ( -- = Metadata Type
    Metadata (..),
    -- = ToMetadata Class
    ToMetadata (..),
    -- = Utility functions
    parseInline',
  )
where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Semigroup (Last (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.Attributes
import Text.AsciiDoc.Inlines

data Metadata = Metadata
  { metadataStyle :: Maybe (Last Text),
    metadataIds :: [Text],
    metadataRoles :: [Text],
    metadataOptions :: [Text],
    metadataTitle :: Maybe (Last Inline),
    metadataPositionalAttributes :: IntMap.IntMap Text,
    -- | Named attributes different than @id@, @opts@, @options@, @role@ and
    -- @title@.
    metadataNamedAttributes :: Map.Map Text Text,
    metadataRoleNamedAttribute :: Maybe (Last [Text])
  }
  deriving (Eq, Show)

instance Semigroup Metadata where
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

instance Monoid Metadata where
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

class ToMetadata a where
  toMetadata :: a -> Metadata

instance ToMetadata (Int, Attribute) where
  toMetadata (index, PositionalAttribute p) =
    mempty {metadataPositionalAttributes = IntMap.singleton index p}
  -- Special treatment of attribute names: @id@, @opts@, @options@, @role@ and
  -- @title@.
  toMetadata (_, NamedAttribute "id" v) =
    mempty {metadataIds = [v]}
  toMetadata (_, NamedAttribute "opts" v) =
    mempty {metadataOptions = [v]}
  toMetadata (_, NamedAttribute "options" v) =
    mempty {metadataOptions = [v]}
  toMetadata (_, NamedAttribute "role" v) =
    mempty
      { metadataRoles = T.words v,
        metadataRoleNamedAttribute = Just $ Last $ T.words v
      }
  toMetadata (_, NamedAttribute "title" v) =
    mempty {metadataTitle = Just $ Last $ parseInline' v}
  -- Any other named attribute
  toMetadata (_, NamedAttribute k v) =
    mempty {metadataNamedAttributes = Map.singleton k v}
  toMetadata (_, ShorthandSyntaxAttribute s i r o) =
    mempty
      { metadataStyle = Just (Last s),
        metadataIds = i,
        metadataRoles = r,
        metadataOptions = o
      }

instance
  {-# OVERLAPPABLE #-}
  (Foldable f, ToMetadata a) =>
  ToMetadata (f a)
  where
  toMetadata = foldMap toMetadata

-- | Stub until proper inline parsing is implemented.
parseInline' :: Text -> Inline
parseInline' = Word
