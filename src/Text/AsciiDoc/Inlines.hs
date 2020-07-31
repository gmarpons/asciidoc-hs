{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
    pInline,
    pInlines,
    Style (..),
    ParameterList(..),
    defaultParameterList,
    parseTest,
  )
where

import Control.Monad
import Control.Monad.Combinators hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators.NonEmpty
import Data.Char hiding (Space)
import qualified Data.List as L
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Debug.Trace as Debug
import qualified Text.Parsec as Parsec
  ( ParseError,
    Parsec,
    eof,
    getPosition,
    getState,
    lookAhead,
    modifyState,
    putState,
    runParser,
    sourceColumn,
    try,
  )
import qualified Text.Parsec.Char as Parsec
  ( anyChar,
    satisfy,
    string,
  )

type Parser = Parsec.Parsec Text State

data State = State
  { -- | A stack (LIFO) of descriptors of potential delimited open inline spans:
    -- e.g., quoted text blocks, sub- or super-scripts, quoted strings. Top of
    -- the stack contains the most recently open span.
    scopes :: [Scope],
    acceptConstrained :: AcceptConstrained
  }
  deriving (Eq, Show)

data AcceptConstrained
  = OpenOnly
  | CloseOnly
  | OpenAndClose
  deriving (Eq, Show)

data Scope
  = Scope String String ScopeApplicability ScopeContent
  deriving (Eq, Ord, Show)

data ScopeApplicability
  = Constrained
  | Unconstrained
  deriving (Eq, Ord, Show)

data ScopeContent
  = Any
  | NoSpaces
  deriving (Eq, Ord, Show)

initialState :: State
initialState =
  State
    { scopes = [],
      acceptConstrained = OpenOnly
    }

defaultScopes :: [Scope]
defaultScopes =
  [ Scope "##" "##" Unconstrained Any,
    Scope "#" "#" Constrained Any,
    Scope "**" "**" Unconstrained Any,
    Scope "*" "*" Constrained Any,
    Scope "__" "__" Unconstrained Any,
    Scope "_" "_" Constrained Any,
    Scope "``" "``" Unconstrained Any,
    Scope "`" "`" Constrained Any
  ]

-- | Association list.
scopeAlternatives :: [(String, String)]
scopeAlternatives =
  [ ("##", "#"),
    ("#", "##"),
    ("**", "*"),
    ("*", "**"),
    ("__", "_"),
    ("_", "__"),
    ("``", "`"),
    ("`", "``")
  ]

openMarker :: Scope -> String
openMarker (Scope open _close _ _) = open

closeMarker :: Scope -> String
closeMarker (Scope _open close _ _) = close

data Inline
  = Space Text
  | Word Text
  | Symbol Text
  | StyledText Style (ParameterList Text) (NonEmpty Inline)
  | InlineSeq (NonEmpty Inline)
  deriving (Eq, Show)

instance Semigroup Inline where
  InlineSeq x <> InlineSeq y = InlineSeq (x <> y)
  InlineSeq x <> y = InlineSeq (appendl x [y])
  x <> InlineSeq y = InlineSeq (x <| y)
  x <> y = InlineSeq (x <| y :| [])

-- > appendl (1 :| [2,3]) [4,5] == 1 :| [2,3,4,5]
appendl :: NonEmpty a -> [a] -> NonEmpty a
appendl (x :| xs) l = x :| (xs ++ l)

data Style
  = Bold
  | Custom
  | Italic
  | Monospace
  deriving (Eq, Show)

makeInline :: Scope -> ParameterList Text -> NonEmpty Inline -> Inline
makeInline scope ps is = case scope of
  Scope "##" _ _ _ -> StyledText Custom ps is
  Scope "#" _ _ _ -> StyledText Custom ps is
  Scope "**" _ _ _ -> StyledText Bold ps is
  Scope "*" _ _ _ -> StyledText Bold ps is
  Scope "__" _ _ _ -> StyledText Italic ps is
  Scope "_" _ _ _ -> StyledText Italic ps is
  Scope "``" _ _ _ -> StyledText Monospace ps is
  Scope "`" _ _ _ -> StyledText Monospace ps is
  _ -> InlineSeq is

pPutAcceptConstrained :: AcceptConstrained -> Parser ()
pPutAcceptConstrained a =
  Parsec.modifyState $ \s -> s {acceptConstrained = a}

pCanAcceptConstrainedOpen :: Parser Bool
pCanAcceptConstrainedOpen = do
  s <- Parsec.getState
  case acceptConstrained s of
    OpenOnly -> pure True
    OpenAndClose -> pure True
    CloseOnly -> pure False

pCanAcceptConstrainedClose :: Parser Bool
pCanAcceptConstrainedClose = do
  s <- Parsec.getState
  case acceptConstrained s of
    OpenOnly -> pure False
    OpenAndClose -> pure True
    CloseOnly -> pure True

-- TODO: no spaces at the beginning.
pInlines :: Parser Inline
pInlines =
  InlineSeq . join <$> some pInline

pInline :: Parser (NonEmpty Inline)
pInline =
  (:| []) <$> pSpace
    <|> (:| []) <$> pWord -- TODO: replace to pBeginWithAlphaNum
    <|> pScopeWithOptionalParameters
    <|> (:| []) <$> pFallback

-- | Like @Text.Parsec.Char.spaces@, but with the following differences:
--
--     * It returns the parsed characters (enclosed in an Inline).
--
--     * Newlines are not considered space.
--
-- It's also different from rules found in
-- https://github.com/Mogztter/asciidoctor-inline-parser/blob/master/lib/asciidoctor/inline_parser/asciidoctor_grammar.treetop
-- in that we include in @pSpaces@ any space character that is not a newline.
pSpace :: Parser Inline
pSpace =
  Space . T.pack . NE.toList
    <$> some pSpaceChar <* pPutAcceptConstrained OpenOnly

pSpaceChar :: Parser Char
pSpaceChar = Parsec.satisfy isAsciiDocSpace

isAsciiDocSpace :: Char -> Bool
isAsciiDocSpace c = isSpace c && c /= '\n'

pWord :: Parser Inline
pWord =
  Word . T.pack . NE.toList
    <$> some pWordChar <* pPutAcceptConstrained CloseOnly
  where
    pWordChar = Parsec.satisfy $ \c ->
      isAlphaNum c

newtype ParameterList a = ParameterList a
  deriving (Eq, Show)

defaultParameterList :: ParameterList Text
defaultParameterList = ParameterList ""

pScopeWithOptionalParameters :: Parser (NonEmpty Inline)
pScopeWithOptionalParameters = do
  canOpenConstrained <- pCanAcceptConstrainedOpen
  -- TODO: parse optional parameter list.
  let parameterList = defaultParameterList
  case canOpenConstrained of
    True ->
      -- Try unconstrained first. If it fails or it is interrupted, assume
      -- constrained will have an equal or better ending.
      Parsec.try (pFailIfInterrupted $ pScope_ Unconstrained parameterList)
        <|> fst <$> pScope_ Constrained parameterList
    False -> fst <$> pScope_ Unconstrained parameterList
  where
    pFailIfInterrupted :: Parser (a, ScopeEnding) -> Parser a
    pFailIfInterrupted p = do
      (a, ending) <- p
      case ending of
        Closed -> pure a
        Interrupted -> empty

data ScopeEnding
  = Closed -- TODO: closing string should be included.
  | Interrupted

pScope_ ::
  ScopeApplicability ->
  ParameterList Text ->
  Parser (NonEmpty Inline, ScopeEnding)
pScope_ applicability ps = do
  when (applicability == Constrained) pCheckAcceptConstrainedOpen
  scope@(Scope open close _applicability _content) <- pPush scopeCandidates
  o <- pOpen open
  i <- pInline
  (is, ending) <- do
    -- GLOBAL PROPERTY: no inline can accept any closing marker if it's not in
    -- its first position, and no inline accepting closing marker characters in
    -- the first position can be processed before `pScope` in `pInline`.
    --
    -- This property is used here to guarantee that the ending token is always
    -- found by `manyTill_`.
    manyTill_ pInline $ do
      b <- pCanAcceptConstrainedClose
      pPop b
  case ending of
    Closed -> do
      c <- pClose close
      pure $ (makeInline scope ps (join (i :| is)) :| [], ending)
    Interrupted -> pure (Symbol (T.pack o) <| join (i :| is), ending)
  where
    pCheckAcceptConstrainedOpen =
      pCanAcceptConstrainedOpen >>= \case
        True -> pure ()
        False -> empty
    pOpen t = Parsec.string t <* pPutAcceptConstrained OpenAndClose
    pClose t = Parsec.string t <* pPutAcceptConstrained OpenAndClose
    scopeCandidates =
      filter (\(Scope _ _ applicability' _) -> applicability == applicability') $
        defaultScopes

pPushDebug arg = do
  -- BEGIN DEBUG
  -- pos <- Parsec.getPosition
  -- nextChar <- pAnd Parsec.anyChar <|> pure 'E'
  -- st <- Parsec.getState
  -- Debug.traceShowM ("Push1", Parsec.sourceColumn pos, nextChar, take 8 (show (acceptConstrained st)), reverse (scopes st))
  -- END DEBUG
  res <- pPush arg
  -- BEGIN DEBUG
  pos <- Parsec.getPosition
  nextChar <- pAnd Parsec.anyChar <|> pure 'E'
  st <- Parsec.getState
  Debug.traceShowM ("Push2" :: String, Parsec.sourceColumn pos, nextChar, take 8 (show (acceptConstrained st)), reverse (scopes st))
  -- END DEBUG
  pure res

-- | It does neither consume input nor modify state field @acceptConstrained@.
--
-- Initial candidates: `scopeCandidates`.
pPush :: [Scope] -> Parser Scope
pPush scopeCandidates = do
  state <- Parsec.getState
  -- All checks under pAnd to not consume input
  found <- choice $ fmap (pAnd . pCheckCandidate state) $ scopeCandidates
  pRulePush2 state found
  -- Modify state, but not acceptConstrained, as we don't consume input.
  Parsec.putState $ state {scopes = found : scopes state}
  pure found
  where
    pCheckCandidate :: State -> Scope -> Parser Scope
    pCheckCandidate state candidate@(Scope _ _ applicability content) = do
      _<- Parsec.string (openMarker candidate)
      case (applicability, content) of
        (Constrained, _) -> pRulePush1
        (_, NoSpaces) -> pRulePush1
        _ -> pure ()
      pRulePush2 state candidate
      pure candidate
    -- RULE PUSH 1: open marker not followed by space, when constrained or the
    -- scope cannot contain spaces. More specifically, find alphanumeric
    -- character before finding space.
    pRulePush1 :: Parser ()
    pRulePush1 = do
      _ <- many (Parsec.satisfy (\c -> not (isSpace c || isAlphaNum c)))
      _ <- Parsec.satisfy isAlphaNum
      pure ()
    -- RULE PUSH 2: alternating nesting. Only admit a scope type already in the
    -- stack if there is a more recent scope of the same type but with different
    -- applicability. E.g.: we can alternate "*" and "**", but not push "*"
    -- again if the most recent "*" is more recent thant the most recent "**";
    -- if "*" does not appear in the stack, we can push.
    pRulePush2 :: State -> Scope -> Parser ()
    pRulePush2 state scope@(Scope open close applicability content) = do
      let scopeAlt = do
            openAlt <- lookup open scopeAlternatives
            pure $ Scope openAlt close applicability content
      let ss = scopes state
      case (scope `L.elemIndex` ss, scopeAlt >>= (`L.elemIndex` ss)) of
        (Just i, Just j) | i > j -> pure ()
        (Nothing, _) -> pure ()
        _ -> empty

pPopDebug arg = do
  -- BEGIN DEBUG
  -- s <- Parsec.getState
  -- pos <- Parsec.getPosition
  -- nextChar <- pAnd Parsec.anyChar <|> pure 'E'
  -- Debug.traceShowM ("Pop 1", Parsec.sourceColumn pos, nextChar, take 8 (show (acceptConstrained s)), reverse (scopes s))
  -- END DEBUG
  res <- pPop arg
  -- BEGIN DEBUG
  s <- Parsec.getState
  pos <- Parsec.getPosition
  nextChar <- pAnd Parsec.anyChar <|> pure 'E'
  Debug.traceShowM ("Pop 2" :: String, Parsec.sourceColumn pos, nextChar, take 8 (show (acceptConstrained s)), reverse (scopes s))
  -- END DEBUG
  pure res

-- | It does neither consume input nor modify state field @acceptConstrained@.
pPop :: Bool -> Parser ScopeEnding
pPop canAcceptConstrainedClose =
  pAnd Parsec.eof *> pure Interrupted
    <|> pPop'
  where
    pPop' :: Parser ScopeEnding
    pPop' = do
      state <- Parsec.getState
      -- All checks under pAnd to not consume input
      found <- choice $ fmap (pAnd . pCheckCandidate state) $ L.tails $ scopes state
      case scopes state of
        [] -> empty -- Cannot happen because `choice` above would have failed.
        top : following -> do
          Parsec.putState $ state {scopes = following}
          case top == found of
            True -> pure Closed
            False -> pure Interrupted
    pCheckCandidate :: State -> [Scope] -> Parser Scope
    pCheckCandidate state = \case
      candidate@(Scope _open close applicability _content) : rest -> do
        -- BEGIN DEBUG
        -- pos <- Parsec.getPosition
        -- nextChar <- pAnd Parsec.anyChar <|> pure 'E'
        -- Debug.traceShowM ("Candidate:" :: String, close, Parsec.sourceColumn pos, nextChar)
        -- END DEBUG
        _ <- Parsec.string close
        case (applicability, canAcceptConstrainedClose) of
          -- pAnd necessary because following RULE POP 1 can consume input.
          (Unconstrained, _) -> pure ()
          (Constrained, True) -> pAnd $ pRulePop1 state
          (Constrained, False) -> empty
        pRulePop2 candidate rest *> pure candidate
      [] -> empty
    -- RULE POP 1: not followed by alphanum. In this case we need to check only
    -- the following character: it must be different from alphanum, or '_'.
    -- There is an exception with '_': when a closing marker starting with '_'
    -- is present in the tail of the stack. This is slightly different to both
    -- what the Asciidoctor documentation says and what Asciidoctor does.
    --
    -- Reference:
    -- https://asciidoctor.org/docs/user-manual/#when-should-i-use-unconstrained-quotes.
    pRulePop1 :: State -> Parser ()
    pRulePop1 state = do
      let exception = case scopes state of
            _ : tail_ ->
              -- Slightly convoluted way to compare heads of close markers to
              -- avoid calling @head@
              any (== '_')
                $ fmap fst
                $ catMaybes
                $ fmap (L.uncons . closeMarker) tail_
            [] -> False
      () <$ Parsec.satisfy (\c -> not (isAlphaNum c || (c == '_' && not exception)))
        <|> Parsec.eof
    -- RULE POP 2: match longest possible marker. Check that we are not popping
    -- a scope with a closing marker that is a prefix of another marker deeper
    -- in the stack that can also be closed. E.g., if we find "**" in the input,
    -- and "*" is in the stack but "**"" also is, we must interrupt the "*"
    -- scope and close the "**" scope.
    pRulePop2 :: Scope -> [Scope] -> Parser ()
    pRulePop2 s ss = do
      let maybeSuffixes =
            NE.nonEmpty
              $ filter (/= "")
              $ catMaybes
              $ fmap (closeMarker s `L.stripPrefix`)
              $ fmap closeMarker ss
      -- BEGIN DEBUG
      -- Debug.traceShowM ("Suffixes" :: String, maybeSuffixes)
      -- END DEBUG
      case maybeSuffixes of
        Just suffixes -> do
          (optional $ choice $ fmap (\t -> Parsec.string t) suffixes) >>= \case
            Just _ -> empty
            Nothing -> do
              -- BEGIN DEBUG
              -- Debug.traceShowM ("Candidate accepted!" :: String, s)
              -- END DEBUG
              pure ()
        Nothing -> pure ()

pFallback :: Parser Inline
pFallback =
  Symbol . T.singleton <$> Parsec.anyChar <* pPutAcceptConstrained OpenAndClose

-- | Models PEG's @&@ operator using Parsec's 'Parsec.lookAhead' and
-- 'Parsec.try'.
pAnd :: Parser a -> Parser a
pAnd p = Parsec.lookAhead (Parsec.try p)

parseTest :: Parser a -> Text -> Either Parsec.ParseError a
parseTest parser text =
  Parsec.runParser parser initialState "" text
