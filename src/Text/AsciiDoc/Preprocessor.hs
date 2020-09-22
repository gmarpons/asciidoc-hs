{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  Text.AsciiDoc.Preprocessor
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- AsciiDoc pre-processor. Expands @include@ directives.
module Text.AsciiDoc.Preprocessor
  ( -- * Datatypes
    IncludeOptions (..),
    AttributeId,
    Line (..),
    PreprocessedLine (..),
    SourceFile (..),

    -- * Pre-process function
    preprocess,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|), NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.Inlines hiding (parseInline)

data IncludeOptions
  = IncludeOptions
  deriving (Eq, Show)

type AttributeId = Text

data Line
  = IncludeLine FilePath IncludeOptions
  | OtherLine Text

data PreprocessedLine
  = OpenAttribute AttributeId [Text]
  | ExpandedAttribute AttributeId Inline
  | EndOfIncludePseudoLine
  | Preprocessed Line
  | Init

data SourceFile
  = MainFile
  | -- | Second value is the starting line of the included block.
    IncludedFile FilePath Int
  deriving (Eq, Show)

-- TODO. Add environment and add attributes to it whenever an ExpandedAttribute
-- is created.

-- TODO. Many of the called funcitons are stubs, in fact includes and attributes
-- are not processed at all
preprocess :: SourceFile -> Text -> IO [PreprocessedLine]
preprocess file t =
  -- Init pseudo-token is created as first current processing line, and then
  -- eliminated at the end of the preprocessing with NE.tail.
  (NE.tail . fst) <$> preprocessRec (lines' t) Init
  where
    lines' :: Text -> [Line]
    lines' = fmap parseIncludeLine . fmap content . linesPeelNewlines
    preprocessRec ::
      [Line] ->
      PreprocessedLine ->
      IO (NonEmpty PreprocessedLine, PreprocessedLine)
    preprocessRec (IncludeLine path options : ls) current = do
      -- TODO. Add include beginning, either at block level or at inline level
      -- (in case of continued attribute).
      includedText <- readInclude path options
      (includedLines, lastIncluded) <-
        preprocessRec (lines' includedText) current
      (remainingLines, lastPreprocessed) <- preprocessRec ls lastIncluded
      pure (includedLines <> (EndOfIncludePseudoLine <| remainingLines), lastPreprocessed)
    preprocessRec (OtherLine lineText : ls) (OpenAttribute k v) = do
      let Wrapped _ v' continuation = peelAttributeContinuation lineText
          next =
            if T.null continuation
              then
                -- TODO. Add newlines.
                -- TODO. Check and improve performance of (v <> [v'])
                ExpandedAttribute k $ parseInline Header $ T.concat (v <> [v'])
              else OpenAttribute k [v']
      preprocessRec ls next
    -- If we do not receive an OpenAttribute as current processing line, we
    -- always add the received line to the returned list.
    preprocessRec (other@(OtherLine lineText) : ls) current = do
      let next = case parseAttribute lineText of
            -- It's an attribute start line
            Just (k, v) ->
              let Wrapped _ v' continuation = peelAttributeContinuation v
               in if T.null continuation
                    then ExpandedAttribute k (parseInline Header v')
                    else (OpenAttribute k [v'])
            -- It's something else
            Nothing -> Preprocessed other
      (remainingLines, lastPreprocessed) <- preprocessRec ls next
      pure (current <| remainingLines, lastPreprocessed)
    preprocessRec [] (OpenAttribute k v) = do
      let final = ExpandedAttribute k (parseInline Header (T.concat v))
      pure (final :| [], final)
    preprocessRec [] final = pure (final :| [], final)
    -- TODO. Stub.
    parseAttribute :: Text -> Maybe (AttributeId, Text)
    parseAttribute _ = Nothing
    -- TODO. Stub.
    parseIncludeLine :: Text -> Line
    parseIncludeLine = OtherLine
    readInclude :: FilePath -> IncludeOptions -> IO Text
    readInclude = readInclude

-- | Function used to know if a text contains an attribute continuation at the
-- end, and get continuation syntax peeled out as part of a Wrapper.
peelAttributeContinuation :: Text -> Wrapped Text Text
peelAttributeContinuation t =
  Wrapped "" (T.dropEnd 2 prefix) (" \\" <> suffix)
  where
    (prefix, suffix) = T.breakOnEnd " \\" t

-- TODO. Can be instance of IsString.
data Wrapped b a
  = Wrapped b a b
  deriving (Eq, Show, Functor)

wrap :: b -> b -> a -> Wrapped b a
wrap before after x = Wrapped before x after

content :: Wrapped b a -> a
content (Wrapped _ x _) = x

-- | Every element type has it's default 'SubstitutionGroup', and there is
-- 'SubstitutionGroup' a default order in which substitutions are applied by all
-- groups. But the default group can be changed for a specific element, and even
-- the order in which substitutions are applied can be altered (e.g. for a
-- specific paragraph). See
-- https://asciidoctor.org/docs/user-manual/#altering-attribute-entry-substitutions
-- (end of the section) for an example.
data SubstitutionGroup
  = -- | Applied to metadata lines in the document header and, in general, to all
    -- attribute entries.
    Header
  | None -- aka Pass or Passthrough
  | Normal
  | Verbatim

-- TODO. This is a stub.
linesPeelNewlines :: Text -> [Wrapped Text Text]
linesPeelNewlines = fmap (wrap "" "") . T.lines

-- TODO. This is a stub.
parseInline :: SubstitutionGroup -> Text -> Inline
parseInline Header = Word
