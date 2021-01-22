{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

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
-- It tries to be compatible with Asciidoctor.
module Text.AsciiDoc.Inlines
  ( Inline (..),
    inlineP,
    inlinesP,
    Style (..),
    ParameterList (..),
    defaultParameterList,
    initialState,
    Parser,
  )
where

import Control.Monad (join, when)
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
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
  ( Parsec,
    char,
    eof,
    getState,
    lookAhead,
    modifyState,
    putState,
    try,
  )
import qualified Text.Parsec.Char as Parsec
  ( anyChar,
    satisfy,
    string,
  )

type Parser = Parsec.Parsec Text State

data State = State
  { -- | A stack (LIFO) of descriptors of open formatting and punctuation marks.
    -- Top of the stack contains the most recently open mark.
    openMarks :: [Mark],
    acceptConstrained :: AcceptConstrained
  }
  deriving (Eq, Show)

data AcceptConstrained
  = OpenOnly
  | CloseOnly
  | OpenAndClose
  deriving (Eq, Show)

-- | A formatting/punctuation pair (of marks) descriptor.
--
-- See
-- https://docs.asciidoctor.org/asciidoc/latest/text/#formatting-terms-and-concepts.
data Mark = Mark
  { openingMark :: String,
    closingMark :: String,
    markType :: MarkType,
    spanContent :: SpanContent
  }
  deriving (Eq, Ord, Show)

data MarkType
  = Constrained
  | Unconstrained
  deriving (Eq, Ord, Show)

data SpanContent
  = Any
  | NoSpaces
  deriving (Eq, Ord, Show)

initialState :: State
initialState =
  State
    { openMarks = [],
      acceptConstrained = OpenOnly
    }

defaultMarks :: [Mark]
defaultMarks =
  [ Mark "##" "##" Unconstrained Any,
    Mark "#" "#" Constrained Any,
    Mark "**" "**" Unconstrained Any,
    Mark "*" "*" Constrained Any,
    Mark "__" "__" Unconstrained Any,
    Mark "_" "_" Constrained Any,
    Mark "``" "``" Unconstrained Any,
    Mark "`" "`" Constrained Any,
    Mark "~" "~" Constrained Any,
    Mark "^" "^" Constrained Any
  ]

-- | Association list.
markAlternatives :: [(String, String)]
markAlternatives =
  [ ("##", "#"),
    ("#", "##"),
    ("**", "*"),
    ("*", "**"),
    ("__", "_"),
    ("_", "__"),
    ("``", "`"),
    ("`", "``")
  ]

data Inline
  = Space Text
  | Word Text
  | Symbol Text
  | Newline Text
  | StyledText Style (ParameterList Text) Text (NonEmpty Inline) Text
  | InlineSeq (NonEmpty Inline)
  deriving (Eq, Show, Typeable, Data)

instance Semigroup Inline where
  InlineSeq x <> InlineSeq y = InlineSeq (x <> y)
  InlineSeq x <> y = InlineSeq (appendl x [y])
  x <> InlineSeq y = InlineSeq (x <| y)
  x <> y = InlineSeq (x <| y :| [])

-- >>> appendl (1 :| [2,3]) [4,5] == 1 :| [2,3,4,5]
appendl :: NonEmpty a -> [a] -> NonEmpty a
appendl (x :| xs) l = x :| (xs ++ l)

data Style
  = Bold
  | Custom
  | Italic
  | Monospace
  | Subscript
  | Superscript
  deriving (Eq, Show, Typeable, Data)

makeInline ::
  Mark ->
  ParameterList Text ->
  Text ->
  NonEmpty Inline ->
  Text ->
  Inline
makeInline mark ps open is close = case mark of
  Mark "##" _ _ _ -> StyledText Custom ps open is close
  Mark "#" _ _ _ -> StyledText Custom ps open is close
  Mark "**" _ _ _ -> StyledText Bold ps open is close
  Mark "*" _ _ _ -> StyledText Bold ps open is close
  Mark "__" _ _ _ -> StyledText Italic ps open is close
  Mark "_" _ _ _ -> StyledText Italic ps open is close
  Mark "``" _ _ _ -> StyledText Monospace ps open is close
  Mark "`" _ _ _ -> StyledText Monospace ps open is close
  Mark "~" _ _ _ -> StyledText Subscript ps open is close
  Mark "^" _ _ _ -> StyledText Superscript ps open is close
  _ -> InlineSeq is

putAcceptConstrainedP :: AcceptConstrained -> Parser ()
putAcceptConstrainedP a =
  Parsec.modifyState $ \s -> s {acceptConstrained = a}

canAcceptConstrainedOpenP :: Parser Bool
canAcceptConstrainedOpenP = do
  s <- Parsec.getState
  case acceptConstrained s of
    OpenOnly -> pure True
    OpenAndClose -> pure True
    CloseOnly -> pure False

canAcceptConstrainedCloseP :: Parser Bool
canAcceptConstrainedCloseP = do
  s <- Parsec.getState
  case acceptConstrained s of
    OpenOnly -> pure False
    OpenAndClose -> pure True
    CloseOnly -> pure True

-- TODO: no spaces at the beginning.
inlinesP :: Parser Inline
inlinesP =
  InlineSeq . join <$> some inlineP

inlineP :: Parser (NonEmpty Inline)
inlineP =
  (:| []) <$> wordP -- TODO: replace to pBeginWithAlphaNum
    <|> (:| []) <$> spaceP
    <|> (:| []) <$> newlineP
    <|> spanWithOptionalParametersP
    <|> (:| []) <$> fallbackP

-- | Like @Text.Parsec.Char.spaces@, but with the following differences:
--
--     * It returns the parsed characters (enclosed in an Inline).
--
--     * Newlines are not considered space.
--
-- It's also different from rules found in
-- https://github.com/Mogztter/asciidoctor-inline-parser/blob/master/lib/asciidoctor/inline_parser/asciidoctor_grammar.treetop
-- in that we include in @pSpaces@ any space character that is not a newline.
spaceP :: Parser Inline
spaceP =
  Space . T.pack . NE.toList
    <$> some spaceCharP <* putAcceptConstrainedP OpenOnly

spaceCharP :: Parser Char
spaceCharP = Parsec.satisfy isAsciiDocSpace

isAsciiDocSpace :: Char -> Bool
isAsciiDocSpace c = isSpace c && c /= '\n'

-- | It parses as newlines the combinations:
--
--     * @CR@
--     * @CR LF@
--     * @LF@
--
-- This is the exact set parsed by @libasciidoc@. At the moment we do not
-- consider the combination @LF CR@ (used in some systems, see
-- https://en.wikipedia.org/wiki/Newline#Representation) as a single newline.
newlineP :: Parser Inline
newlineP =
  Newline <$> newlineP' <* putAcceptConstrainedP OpenOnly
  where
    newlineP' :: Parser Text
    newlineP' =
      (<>) <$> singletonP '\r' <*> option "" (singletonP '\n')
        <|> singletonP '\n'
    singletonP :: Char -> Parser Text
    singletonP c = T.singleton <$> Parsec.char c

wordP :: Parser Inline
wordP =
  Word . T.pack . NE.toList
    <$> some wordCharP <* putAcceptConstrainedP CloseOnly
  where
    wordCharP = Parsec.satisfy $ \c ->
      isAlphaNum c

newtype ParameterList a = ParameterList a
  deriving (Eq, Show, Data, Typeable)

defaultParameterList :: ParameterList Text
defaultParameterList = ParameterList ""

parameterListP :: Parser (ParameterList Text)
parameterListP =
  ParameterList . T.pack
    <$ Parsec.char '[' <*> manyTill Parsec.anyChar (Parsec.char ']')

spanWithOptionalParametersP :: Parser (NonEmpty Inline)
spanWithOptionalParametersP = Parsec.try $ do
  canOpenConstrained <- canAcceptConstrainedOpenP
  maybeParameters <- optional parameterListP
  let parameterList = maybe defaultParameterList id maybeParameters
  (is, ending) <- case canOpenConstrained of
    True ->
      -- Try unconstrained first. If it fails or it is interrupted, assume
      -- constrained will have an equal or better ending.
      (\is -> (is, Closed))
        <$> Parsec.try (failIfInterruptedP $ spanP_ Unconstrained parameterList)
          <|> spanP_ Constrained parameterList
    False -> spanP_ Unconstrained parameterList
  case (ending, maybeParameters) of
    (Closed, Just _) -> pure is
    (_, Nothing) -> pure is
    (Interrupted, Just _) -> empty

failIfInterruptedP :: Parser (a, SpanEnding) -> Parser a
failIfInterruptedP p = do
  (a, ending) <- p
  case ending of
    Closed -> pure a
    Interrupted -> empty

data SpanEnding
  = Closed -- TODO: closing string should be included.
  | Interrupted

spanP_ ::
  MarkType ->
  ParameterList Text ->
  Parser (NonEmpty Inline, SpanEnding)
spanP_ mt ps = do
  when (mt == Constrained) checkAcceptConstrainedOpenP
  mark@Mark {openingMark, closingMark} <- pushP markCandidates
  o <- openP openingMark
  i <- inlineP
  (is, ending) <- do
    -- GLOBAL PROPERTY: no inline can accept any closing mark if it's not in
    -- its first position, and no inline accepting closing mark characters in
    -- the first position can be processed before `pSpan` in `inlineP`.
    --
    -- This property is used here to guarantee that the ending token is always
    -- found by `manyTill_`.
    manyTill_ inlineP $ do
      b <- canAcceptConstrainedCloseP
      popP b
  case ending of
    Closed -> do
      c <- closeP closingMark
      pure $
        (makeInline mark ps (T.pack o) (join (i :| is)) (T.pack c) :| [], ending)
    Interrupted -> pure (Symbol (T.pack o) <| join (i :| is), ending)
  where
    checkAcceptConstrainedOpenP =
      canAcceptConstrainedOpenP >>= \case
        True -> pure ()
        False -> empty
    openP t = Parsec.string t <* putAcceptConstrainedP OpenAndClose
    closeP t = Parsec.string t <* putAcceptConstrainedP OpenAndClose
    markCandidates =
      filter (\mark -> markType mark == mt) $
        defaultMarks

-- pPushDebug arg = do
--   pos <- Parsec.getPosition
--   nextChar <- andP Parsec.anyChar <|> pure 'E'
--   st <- Parsec.getState
--   Debug.traceShowM ("Debug. Push" :: String, Parsec.sourceColumn pos, nextChar, take 8 (show (acceptConstrained st)), reverse (openMarks st))
--   res <- pushP arg
--   pure res

-- | It does neither consume input nor modify state field @acceptConstrained@.
--
-- Initial candidates: `markCandidates`.
pushP :: [Mark] -> Parser Mark
pushP markCandidates = do
  state <- Parsec.getState
  -- All checks under andP to not consume input
  found <- choice $ fmap (andP . pCheckCandidate state) $ markCandidates
  pRulePush2 state found
  -- Modify state, but not acceptConstrained, as we don't consume input.
  Parsec.putState $ state {openMarks = found : openMarks state}
  pure found
  where
    pCheckCandidate :: State -> Mark -> Parser Mark
    pCheckCandidate state candidate = do
      -- Debug.traceShowM ("Debug. Push candidate: ", candidate)
      _ <- Parsec.string (openingMark candidate)
      case (markType candidate, spanContent candidate) of
        (Constrained, _) -> pRulePush1
        (_, NoSpaces) -> pRulePush1
        _ -> pure ()
      -- pRulePush2 state candidate
      -- Debug.traceShowM ("Debug. Accept push candidate: ", candidate)
      pure candidate
    -- RULE PUSH 1: open mark not followed by space, when constrained or the
    -- mark cannot contain spaces. More specifically, find alphanumeric
    -- character before finding space.
    pRulePush1 :: Parser ()
    pRulePush1 = do
      _ <- many (Parsec.satisfy (\c -> not (isSpace c || isAlphaNum c)))
      _ <- Parsec.satisfy isAlphaNum
      pure ()
    -- RULE PUSH 2: alternating nesting. Only admit a mark type already in the
    -- stack if there is a more recent mark of the same type but with different
    -- mark type. E.g.: we can alternate "*" and "**", but not push "*" again if
    -- the most recent "*" is more recent thant the most recent "**"; if "*"
    -- does not appear in the stack, we can push.
    pRulePush2 :: State -> Mark -> Parser ()
    pRulePush2 state mark@Mark {openingMark, closingMark, markType, spanContent} = do
      let spanAlt = do
            openAlt <- lookup openingMark markAlternatives
            pure $ Mark openAlt closingMark markType spanContent
      let ss = openMarks state
      case (mark `L.elemIndex` ss, spanAlt >>= (`L.elemIndex` ss)) of
        (Just i, Just j) | i > j -> pure ()
        (Nothing, _) -> pure ()
        _ -> empty

-- pPopDebug arg = do
--   s <- Parsec.getState
--   pos <- Parsec.getPosition
--   nextChar <- andP Parsec.anyChar <|> pure 'E'
--   Debug.traceShowM ("Debug. Pop" :: String, Parsec.sourceColumn pos, nextChar, take 8 (show (acceptConstrained s)), reverse (openMarks s))
--   res <- popP arg
--   pure res

-- | It does neither consume input nor modify state field @acceptConstrained@.
--
-- TODO: Avoid boolean blindness in argument.
popP :: Bool -> Parser SpanEnding
popP canAcceptConstrainedClose =
  andP Parsec.eof *> pure Interrupted
    <|> popP'
  where
    popP' :: Parser SpanEnding
    popP' = do
      state <- Parsec.getState
      -- All checks under andP to not consume input
      found <-
        choice $ fmap (andP . pCheckCandidate state) $ L.tails $ openMarks state
      case openMarks state of
        [] -> empty -- Cannot happen because above `candidates` calculation would have failed.
        top : tail_ -> do
          Parsec.putState $ state {openMarks = tail_}
          case top == found of
            True -> pure Closed
            False -> pure Interrupted
    pCheckCandidate :: State -> [Mark] -> Parser Mark
    pCheckCandidate state = \case
      candidate : tail_ -> do
        -- Debug.traceShowM ("Debug. Pull candidate: ", candidate, canAcceptConstrainedClose)
        _ <- Parsec.string $ closingMark candidate
        case (markType candidate, canAcceptConstrainedClose) of
          -- Nested andP necessary because following RULE POP 1 can consume input.
          (Unconstrained, _) -> pure ()
          (Constrained, True) -> andP $ pRulePop1 state
          (Constrained, False) -> empty
        pRulePop2 candidate tail_
        -- Debug.traceShowM ("Debug. Accept pull candidate: ", candidate)
        pure candidate
      [] -> empty
    -- RULE POP 1: not followed by alphanum. In this case we need to check only
    -- the following character: it must be different from alphanum, or '_'.
    -- There is an exception with '_': when a closing mark starting with '_'
    -- is present in the tail of the stack. This is slightly different to both
    -- what the Asciidoctor documentation says and what Asciidoctor does.
    --
    -- Reference:
    -- https://asciidoctor.org/docs/user-manual/#when-should-i-use-unconstrained-quotes.
    pRulePop1 :: State -> Parser ()
    pRulePop1 state = do
      let exception = case openMarks state of
            _ : tail_ ->
              -- Slightly convoluted way to compare heads of closing marks to
              -- avoid calling @head@
              any (== '_') $
                fmap fst $
                  catMaybes $
                    fmap (L.uncons . closingMark) tail_
            [] -> False
      () <$ Parsec.satisfy (\c -> not (isAlphaNum c || (c == '_' && not exception)))
        <|> Parsec.eof
    -- RULE POP 2: match longest possible mark. Check that we are not popping
    -- a mark with a closing mark that is a prefix of another mark deeper
    -- in the stack that can also be closed. E.g., if we find "**" in the input,
    -- and "*" is in the stack but "**"" also is, we must interrupt the "*"
    -- mark and close the "**" mark.
    pRulePop2 :: Mark -> [Mark] -> Parser ()
    pRulePop2 s ss = do
      let maybeSuffixes =
            NE.nonEmpty $
              filter (/= "") $
                catMaybes $
                  fmap (closingMark s `L.stripPrefix`) $
                    fmap closingMark ss
      case maybeSuffixes of
        Just suffixes -> do
          (optional $ choice $ fmap (\t -> Parsec.string t) suffixes) >>= \case
            Just _ -> empty
            Nothing -> do
              pure ()
        Nothing -> pure ()

fallbackP :: Parser Inline
fallbackP =
  Symbol . T.singleton <$> Parsec.anyChar <* putAcceptConstrainedP OpenAndClose

-- | Models PEG's @&@ operator using Parsec's 'Parsec.lookAhead' and
-- 'Parsec.try'.
andP :: Parser a -> Parser a
andP p = Parsec.lookAhead (Parsec.try p)
