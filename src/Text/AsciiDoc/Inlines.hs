{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.AsciiDoc.Inlines
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains Parsec-style parsers for AsciiDoc inline elements.
--
-- It tries to be compatible with Asciidoctor, but uses a grammar-based parsing
-- approach instead of regexes.
--
-- There are three kinds of terminals in the grammar:
--
-- * Alpha-numeric sequences ('AlphaNum').
-- * Gaps: space-like character sequences and 'Newline's (including the
--   surrounding space).
-- * Other characters: punctuation symbols, mathematical symbols, etc.
--   It includes formatting and punctuation marks.
--
-- These groups of characters govern how constrained enclosures are parsed.
module Text.AsciiDoc.Inlines
  ( -- * AST types
    Inline (..),
    Style (..),
    InlineAttributeList (..),
    defaultAttributeList,

    -- * Parsers
    inlinesP,

    -- * Parser type
    Parser,
    State (..),
    inlineParserInitialState,
  )
where

import Control.Monad (when)
import Control.Monad.Combinators (someTill)
import Control.Monad.Combinators hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators.NonEmpty (some)
import Data.Char (isAlphaNum, isSpace)
import Data.Generics (Data, Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.ElementAttributes
import Text.AsciiDoc.Metadata
import Text.AsciiDoc.SpecialChars
import Text.AsciiDoc.UnparsedInline
import Text.Parsec ((<?>))
import qualified Text.Parsec as Parsec
  ( ParsecT,
    char,
    eof,
    getState,
    label,
    lookAhead,
    notFollowedBy,
    parse,
    putState,
    try,
  )
import qualified Text.Parsec.Char as Parsec
  ( anyChar,
    satisfy,
    string,
  )

type Parser m = Parsec.ParsecT Text State m

-- | Custom parser state for the parser for 'Inline's.
newtype State = State
  { -- | A stack (LIFO) of formatting and punctuation 'Mark's.
    --
    -- Every mark in the stack is the opening mark for the corresponding
    -- enclosure.
    -- Top of the stack contains the opening mark for most recently open
    -- enclosure.
    openEnclosures :: [Mark]
  }
  deriving newtype (Eq, Show)

inlineParserInitialState :: State
inlineParserInitialState =
  State
    { openEnclosures = []
    }

-- | Formatting styles for inline text.
data Style
  = Bold
  | Custom
  | Italic
  | Monospace
  deriving stock (Eq, Show, Typeable, Data)

--  | Subscript
--  | Superscript

-- | Every formatting 'Mark' maps to a 'Style', and this function computes this
-- map.
toStyle :: Mark -> Style
toStyle = \case
  SingleMark NumberF -> Custom
  SingleMark AsteriskF -> Bold
  SingleMark UnderscoreF -> Italic
  SingleMark GraveF -> Monospace
  DoubleMark NumberF -> Custom
  DoubleMark AsteriskF -> Bold
  DoubleMark UnderscoreF -> Italic
  DoubleMark GraveF -> Monospace

-- | A data type for all the different inline types.
--
-- Some inline types can contain other inlines, and there is a constructor
-- 'InlineSeq' serving as a general inline container.
data Inline
  = AlphaNum Text
  | EndOfInline Text
  | InlineSeq (NonEmpty Inline)
  | Newline Text
  | Space Text
  | StyledText Style InlineAttributeList Text (NonEmpty Inline) Text
  | Symbol Text
  deriving stock (Eq, Show, Typeable, Data)

--  | InlineMacro Text
--  | EscapedSymbol Text
--  | DoubleEscapedSymbol Text

-- EBNF grammar non-terminal symbols  ------------------------------------------

-- The parser can be read as an EBNF grammar, with starting symbol 'inlinesP'.
-- The resulting grammar would be ambiguous, so there are a series of functions
-- used to discard some parsing paths ('muP', 'piP', 'sigmaP', 'phiP', 'psiP',
-- 'omegaP'), that work together with order in alternatives to give a completely
-- deterministic parser.

-- | This function is the only exported parser function for AsciiDoc inlines.
--
-- If more than one individual inlines can be parsed, it returns all of them
-- encapsulated into an 'InlineSeq'.
inlinesP :: Monad m => Parser m Inline
inlinesP =
  (\(x :| xs) ys -> InlineSeq (x :| xs ++ ys))
    <$> (firstP <?> "F")
      <*> ( concat
              <$> many
                ( NE.toList <$> Parsec.label gapWithOptionalContinuationP "N1"
                    <|> Parsec.label (sigmaP *> nonGapSequenceP) "N2"
                )
          )
      <* Parsec.eof

unconstrainedP :: Monad m => Parser m Inline
unconstrainedP = Parsec.try $ do
  Parsec.label (pure ()) "U"
  ps <- option defaultAttributeList inlineAttributeListP
  phiP
  openMark <- openP ["##", "**", "__", "``"]
  is <- inlinesInUnconstrainedP
  closeMark <- closeP openMark
  pure $ StyledText (toStyle openMark) ps (fromMarkT openMark) is (fromMarkT closeMark)
  where
    fromMarkT = T.pack . fromMark
    inlinesInUnconstrainedP =
      (\(x :| xs) ys -> x :| xs ++ ys)
        <$> ((gapWithOptionalContinuationP <|> firstP) <?> "Y_p | F")
        <*> ( concat
                <$> many
                  ( NE.toList <$> Parsec.label gapWithOptionalContinuationP "N1"
                      <|> Parsec.label (sigmaP *> nonGapSequenceP) "N2"
                  )
            )
{-# ANN unconstrainedP ("HLint: ignore" :: String) #-}

constrainedP :: Monad m => Parser m Inline
constrainedP = Parsec.try $ do
  Parsec.label (pure ()) "C"
  ps <- option defaultAttributeList inlineAttributeListP
  varphiP
  openMark <- openP ["#", "*", "_", "`"]
  is <- inlinesInConstrainedP
  omegaP
  closeMark <- closeP openMark
  pure $ StyledText (toStyle openMark) ps (fromMarkT openMark) is (fromMarkT closeMark)
  where
    fromMarkT = T.pack . fromMark
    inlinesInConstrainedP =
      (\(x :| xs) ys -> x :| xs ++ ys)
        <$> (firstP <?> "F")
        <*> ( concat
                <$> many
                  ( Parsec.label gapWithContinuationP "N1"
                      <|> Parsec.label (muP *> nonGapSequenceP) "N2"
                  )
            )
{-# ANN constrainedP ("HLint: ignore" :: String) #-}

firstP :: Monad m => Parser m (NonEmpty Inline)
firstP =
  -- Notice the similarity with alphaNumOrOtherP.
  (:| [])
    <$> alphaNumP
    <|> piP *> otherWithContinuationP

nonGapSequenceP :: Monad m => Parser m [Inline]
nonGapSequenceP =
  (:)
    <$> (unconstrainedP <|> otherP)
    <*> option [] alphaNumOrOtherP

openP :: Monad m => [Mark] -> Parser m Mark
openP ms = do
  Parsec.label (pure ()) "M_>"
  mark <- choice $ Parsec.try . markP <$> ms
  -- What follows is not part of the EBNF description of the language, but it's
  -- easier to put it here than create a specific function for it.
  st <- Parsec.getState
  Parsec.putState $ st {openEnclosures = mark : openEnclosures st}
  Parsec.label (pure ()) $ "State: " ++ show (mark : openEnclosures st)
  pure mark

closeP :: Monad m => Mark -> Parser m Mark
closeP openMark = do
  -- Passing a mark to this function is redundant, but the openP/closeP
  -- connection makes the interface more clear for callers. It can also be used
  -- to (run-time) check for some programming errors.
  Parsec.label (pure ()) $ "M_<: " ++ show openMark
  let closeMark = closingMarkOf openMark
  _ <- markP closeMark
  -- What follows is not part of the EBNF description of the language, but it's
  -- easier to put it here than create a specific function for it.
  st <- Parsec.getState
  case openEnclosures st of
    (e : es) -> do
      when (closingMarkOf e /= closeMark) $
        error "closeP: trying to close mark different from the innermost enclosure"
      Parsec.putState $ st {openEnclosures = es}
    [] -> error "closeP: trying to close non-existent enclosure"
  pure closeMark

gapWithContinuationP :: Monad m => Parser m [Inline]
gapWithContinuationP =
  flip Parsec.label "Y" $
    -- Parsec.try is necessary because we can accept gapP and fail afterwards.
    Parsec.try $
      (++)
        <$> (NE.toList <$> gapP)
        <*> ((: []) <$> alphaNumP <|> NE.toList <$ sigmaP <*> otherWithContinuationP)

gapWithOptionalContinuationP :: Monad m => Parser m (NonEmpty Inline)
gapWithOptionalContinuationP =
  flip Parsec.label "Y_p" $
    (\(h :| t) c -> h :| t ++ c)
      <$> gapP
      <*> option [] ((: []) <$> alphaNumP <|> NE.toList <$ sigmaP <*> otherWithContinuationP)

otherWithContinuationP :: Monad m => Parser m (NonEmpty Inline)
otherWithContinuationP =
  flip Parsec.label "X" $
    ((:|) <$> unconstrainedP <*> option [] alphaNumOrOtherP)
      <|> ((:|) <$> constrainedP <*> option [] gapOrOtherWithContinuationP)
      <|> ((:|) <$> otherP <*> option [] alphaNumOrOtherP)
  where
    gapOrOtherWithContinuationP =
      flip Parsec.label "Y | X" $
        gapWithContinuationP
          <|> muP *> (NE.toList <$> otherWithContinuationP)

alphaNumOrOtherP :: Monad m => Parser m [Inline]
alphaNumOrOtherP =
  flip Parsec.label "A | X" $
    (: [])
      <$> alphaNumP
      <|> muP *> (NE.toList <$> otherWithContinuationP)

gapP :: Monad m => Parser m (NonEmpty Inline)
gapP = flip Parsec.label "G" $ some newlineOrSpaceP
  where
    newlineOrSpaceP =
      newlineP
        -- Inline the definition of spaceP here allows to avoid redundant checks
        -- against '\n' or '\r'.
        <|> Space . T.pack . NE.toList <$> some (Parsec.satisfy isSpace)

-- Functions for disambiguating the EBNF grammar  ------------------------------

-- | Function called after a character of kind alphanum or other, and before a
-- character of kind other.
--
-- TODO. Check that this function together with 'sigmaP' cover all cases at the
-- beginning of N
--
-- It fails if an open enclosure can be closed (using 'closableMarks'), or a
-- full unconstrained enclosure can be parsed, at current input.
muP :: Monad m => Parser m ()
muP = do
  Parsec.label (pure ()) "MU"
  st <- Parsec.getState
  Parsec.notFollowedBy (choice $ tryToCloseMarkP <$> closableMarks (openEnclosures st))
    <|> Parsec.lookAhead (() <$ unconstrainedP)

-- | Function called after the opening mark of an enclosure (i.e., after a
-- character of kind other), and before a character of kind other.
--
-- It's identical to 'muP' with the exception that the mark recently open (top
-- of the stack) is not taken into account.
--
-- It fails if an open enclosure (except the innermost one) can be closed (using
-- 'closableMarks'), or a full unconstrained enclosure can be parsed, at current
-- input.
piP :: Monad m => Parser m ()
piP = do
  Parsec.label (pure ()) "PI"
  st <- Parsec.getState
  case openEnclosures st of
    (_ : es) ->
      Parsec.notFollowedBy (choice $ tryToCloseMarkP <$> closableMarks es)
        <|> Parsec.lookAhead (() <$ unconstrainedP)
    [] -> pure ()

-- | Function called after a character of kind gap, and before a character of
-- kind other.
--
-- It fails if any unconstrained open enclosure can be closed.
sigmaP :: Monad m => Parser m ()
sigmaP = do
  Parsec.label (pure ()) "SIGMA"
  st <- Parsec.getState
  Parsec.notFollowedBy $
    choice $ tryToCloseMarkP <$> filter isUnconstrained (openEnclosures st)

-- | Function called before the opening mark for an unconstrained enclosure
-- (i.e., before a character of kind other)
--
-- It fails if we're trying to open an already open mark.
--
-- It takes into account the case that we can open both an arleady open mark and
-- an extension of it (e.g., "@**@" is an extension of "@*@"). This function
-- doesn't fail in this case, and the parser will try to open the extended mark.
phiP :: Monad m => Parser m ()
phiP = do
  Parsec.label (pure ()) "PHI"
  st <- Parsec.getState
  Parsec.notFollowedBy (choice $ markP <$> openEnclosures st)
    <|> () <$ Parsec.lookAhead (choice $ markP <$> concatMap extendedMarksOf (openEnclosures st))

-- | Function called before the opening mark for a constrained enclosure (i.e.,
-- before a character of kind other).
--
-- It fails if we're trying to open an already open mark.
varphiP :: Monad m => Parser m ()
varphiP = do
  Parsec.label (pure ()) "VARPHI"
  st <- Parsec.getState
  Parsec.notFollowedBy $ choice $ markP <$> openEnclosures st

-- | Function called after a character of kind alphanum or other, and before the
-- closing mark for a constrained enclosure (i.e., before a character of kind
-- other).
--
-- It fails if we can close an open extended version (i.e., an unconstrained
-- enclosure) of the mark we're trying to close, as closing the unconstrained
-- enclosure takes priority.
omegaP :: Monad m => Parser m ()
omegaP = do
  Parsec.label (pure ()) "OMEGA"
  st <- Parsec.getState
  Parsec.notFollowedBy $
    choice $ tryToCloseMarkP <$> filter isUnconstrained (openEnclosures st)

closableMarks :: [Mark] -> [Mark]
closableMarks ms = x ++ filter isUnconstrained y
  where
    (x, y) = span isConstrained ms

tryToCloseMarkP :: Monad m => Mark -> Parser m ()
tryToCloseMarkP m = Parsec.try $ do
  Parsec.label (pure ()) $ "tryToCloseMarkP: " ++ show m
  _ <- markP $ closingMarkOf m
  when (isConstrained m) $ do
    Parsec.eof <|> () <$ Parsec.satisfy (not . isAlphaNum)

-- EBNF grammar terminal symbols  ----------------------------------------------

markP :: Monad m => Mark -> Parser m Mark
markP m = m <$ Parsec.string (fromMark m)

otherP :: Monad m => Parser m Inline
otherP =
  Symbol . T.singleton
    <$> Parsec.satisfy (\c -> not (isSpace c || isAlphaNum c))

-- | It parses as newlines the combinations:
--
--     * @CR@
--     * @CR LF@
--     * @LF@
--
-- This is the exact set parsed by @libasciidoc@. At the moment we do not
-- consider the combination @LF CR@ (used in some systems, see
-- https://en.wikipedia.org/wiki/Newline#Representation) as a single newline.
newlineP :: Monad m => Parser m Inline
newlineP =
  wrap <$> newlineP' <*> optional Parsec.eof
  where
    wrap t Nothing = Newline t
    wrap t (Just ()) = EndOfInline t
    newlineP' :: Monad m => Parser m Text
    newlineP' =
      (<>) <$> singletonP '\r' <*> option "" (singletonP '\n')
        <|> singletonP '\n'
    singletonP :: Monad m => Char -> Parser m Text
    singletonP c = T.singleton <$> Parsec.char c

alphaNumP :: Monad m => Parser m Inline
alphaNumP =
  AlphaNum . T.pack . NE.toList
    <$> Parsec.label (some wordCharP) "A"
  where
    wordCharP = Parsec.satisfy $ \c ->
      isAlphaNum c

-- Parser for element attribute (aka parameter) lists  ------------------------

newtype InlineAttributeList = InlineAttributeList Text
  deriving newtype (Eq, Show)
  deriving stock (Data, Typeable)

-- | This instance accepts the same kind of attributes than the instance for
-- 'Text.AsciiDoc.Blocks.BlockPrefixItem's, including the shorthand syntax.
--
-- Attributes @title@ and @opts@/@options@ have currently no meaning for
-- inlines, but they are still parsed and stored in the resulting 'Metadata'
-- value.
--
-- __Divergence from Asciidoctor__: The aforementioned behavior implies that
-- some inputs produce different results than Asciidoctor.
-- Asciidoctor only honours @role@ and @id@ attributes and messes up the rest.
instance ToMetadata InlineAttributeList UnparsedInline where
  toMetadata (InlineAttributeList "") = mempty
  toMetadata (InlineAttributeList t) =
    case Parsec.parse attributeListP "" t of
      Right attributes ->
        toMetadata $ PositionedAttribute <$> NE.zip (1 :| [2 ..]) attributes
      Left _ -> error "toMetadata @InlineAttributeList: parse should not fail"

defaultAttributeList :: InlineAttributeList
defaultAttributeList = InlineAttributeList ""

-- | Accepts an square-bracket-enclosed string with no restrictions on the
-- characters in between, provided that there is at least one such character.
inlineAttributeListP :: Monad m => Parser m InlineAttributeList
inlineAttributeListP =
  flip Parsec.label "P" $
    InlineAttributeList . T.pack
      <$ Parsec.char '[' <*> someTill Parsec.anyChar (Parsec.char ']')
