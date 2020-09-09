-- |
-- Module      :  Text.AsciiDoc.Inlines Copyright   :  © 2020–present Guillem
-- Marpons License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org> Stability   :
-- experimental Portability :  portable
--
-- This module contains functions that traverse an AsciiDoc AST and decorate it
-- with source ranges information.
module Text.AsciiDoc.SourceRange
  ( SourcePosition,
    SourceRange (..),
    addSourceRanges,
    childInlines,
    contentLength,
  )
where

import Data.List.NonEmpty
import qualified Data.Text as T
import qualified Optics.Core as Optics
import Optics.Traversal (Traversal')
import Text.AsciiDoc.Inlines

-- | Extract the immediate descendants (sub-inlines) of an `Inline`.
--
-- Analogous to @Lens.Plated.children@.
childInlines :: Traversal' Inline Inline
childInlines = Optics.traversalVL subInlines'
  where
    subInlines' f = \case
      StyledText style parameters open inlines close ->
        fmap (\x -> StyledText style parameters open x close) $ traverse f inlines
      InlineSeq inlines -> InlineSeq <$> traverse f inlines
      x -> pure x

contentLength :: Inline -> Int
contentLength = \case
  Space t -> T.length t
  Word t -> T.length t
  Symbol t -> T.length t
  Newline t -> T.length t
  _ -> 0

type SourcePosition = (Int, Int)

data SourceRange = SourceRange SourcePosition SourcePosition

instance Show SourceRange where
  show (SourceRange (l1, c1) (l2, c2)) =
    show l1 <> ":" <> show c1 <> "-" <> show l2 <> ":" <> show c2

-- | TODO: maybe this function should be idempotent.
addSourceRanges :: Inline -> Inline
addSourceRanges = fst . addSourceRanges' (1, 1)
  where
    addSourceRanges' :: SourcePosition -> Inline -> (Inline, SourcePosition)
    addSourceRanges' initial@(initialLine, initialColumn) x =
      let (x', (finalLine', finalColumn')) =
            Optics.mapAccumLOf
              childInlines
              addSourceRanges'
              (initialLine, initialColumn + prefixLength x)
              x
          final@(finalLine, finalColumn) = case x of
            -- `final` is the _previous_ position to the first position of the
            -- next inline, so we use 0 as column value (we increment by one the
            -- value returned by this function).
            Newline _ -> (finalLine' + 1, 0)
            _ -> (finalLine', finalColumn' + contentLength x + suffixLength x - 1)
       in (wrap (SourceRange initial final) x', (finalLine, finalColumn + 1))
    wrap :: SourceRange -> Inline -> Inline
    wrap range = \case
      x@(Newline _) -> x
      x@(Word _) ->
        StyledText
          Custom
          (ParameterList (T.pack $ "data-sourcepos: " <> show range))
          ""
          (x :| [])
          ""
      x -> x
    prefixLength = \case
      StyledText _ (ParameterList t) o _ _
        | T.null t -> T.length o
        | otherwise -> 2 + T.length t + T.length o
      _ -> 0
    suffixLength = \case
      StyledText _ _ _ _ c -> T.length c
      _ -> 0
