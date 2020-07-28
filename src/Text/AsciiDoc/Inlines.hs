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
    -- Inlines,
    -- Format (..),
    pInline,
    pInlines,
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
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import qualified Text.Parsec as Parsec
  ( ParseError,
    Parsec,
    ParsecT,
    Stream,
    eof,
    getState,
    lookAhead,
    modifyState,
    notFollowedBy,
    putState,
    runParser,
    try,
    unexpected,
  )
import qualified Text.Parsec.Char as Parsec
  ( anyChar,
    char,
    oneOf,
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
  [ Scope "\"`" "`\"" Constrained Any,
    Scope "##" "##" Unconstrained Any,
    Scope "'`" "`'" Constrained Any,
    Scope "#" "#" Constrained Any,
    Scope "**" "**" Unconstrained Any,
    Scope "*" "*" Constrained Any,
    Scope "+++" "+++" Unconstrained Any,
    Scope "++" "++" Unconstrained Any,
    Scope "+" "+" Constrained Any,
    Scope "^" "^" Unconstrained NoSpaces,
    Scope "__" "__" Unconstrained Any,
    Scope "_" "_" Constrained Any,
    Scope "``" "``" Unconstrained Any,
    Scope "`" "`" Constrained Any,
    Scope "~" "~" Unconstrained NoSpaces
  ]

defaultConstrainedScopes :: [Scope]
defaultConstrainedScopes = filter p defaultScopes
  where
    p (Scope _ _ applicability _) = applicability == Constrained

defaultUnconstrainedScopes :: [Scope]
defaultUnconstrainedScopes = filter p defaultScopes
  where
    p (Scope _ _ applicability _) = applicability == Unconstrained

data Inline
  = Space Text
  | Word Text
  | Punctuation Text
  | SpecialPunctuation Text
  | Newline Text
  | Symbol Text
  | StyledText Style ParameterList (NonEmpty Inline)
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

makeInline :: Scope -> ParameterList -> NonEmpty Inline -> Inline
makeInline scope ps is = case scope of
  Scope "##" _ _ _ -> StyledText Custom ps is
  Scope "#" _ _ _ -> StyledText Custom ps is
  Scope "**" _ _ _ -> StyledText Bold ps is
  Scope "*" _ _ _ -> StyledText Bold ps is
  Scope "__" _ _ _ -> StyledText Italic ps is
  Scope "_" _ _ _ -> StyledText Italic ps is
  Scope "``" _ _ _ -> StyledText Monospace ps is
  Scope "`" _ _ _ -> StyledText Monospace ps is

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

pInlines :: Parser Inline
pInlines =
  InlineSeq <$> some pInline

pInline :: Parser Inline
pInline =
  pSpace
    <|> pWord -- TODO: replace to pBeginWithAlphaNum
    <|> pScope
    <|> pFallback

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

newtype ParameterList = ParameterList Text
  deriving (Eq, Show)

defaultParameterList :: ParameterList
defaultParameterList = ParameterList ""

pPreParameterList :: Parser (Either Char ParameterList)
pPreParameterList = pParameterList_

pPostParameterList :: Parser (Either Char ParameterList)
pPostParameterList = pParameterList_

pParameterList_ :: Parser (Either Char ParameterList)
pParameterList_ =
  Right <$> Parsec.try pParameterList'
    <|> Left <$> Parsec.char '['
  where
    pParameterList' =
      ParameterList . T.pack . NE.toList
        <$ Parsec.char '[' <*> someTill Parsec.anyChar (Parsec.char ']')

-- Can return '[' or marker, or scope.
pScope :: Parser Inline
pScope = do
  canOpenConstrained <- pCanAcceptConstrainedOpen
  option (Right defaultParameterList) pPreParameterList >>= \case
    -- Wrong parameter list, consume the '[' char and return:
    Left c -> do
      pure $ Symbol (T.singleton c)
    Right parameterList -> do
      is <- case canOpenConstrained of
        True ->
          Parsec.try (pUnconstrained_ parameterList)
            <|> pConstrained_ parameterList
        False -> pUnconstrained_ parameterList
      pure is

data ScopeEnding
  = Closed
  | Interrupted

pUnconstrained_ :: ParameterList -> Parser Inline
pUnconstrained_ =
  -- TODO
  pConstrained_

pConstrained_ :: ParameterList -> Parser Inline
pConstrained_ ps = do
  scope@(Scope open close _applicability _content) <- pPush
  o <- pOpen open
  i <- pInline
  (is, ending) <-
    manyTill_ pInline (pCanAcceptConstrainedClose *> pPull)
  case ending of
    Closed -> do
      c <- pClose close
      pure $ makeInline scope ps (i :| is)
    Interrupted -> pure $ InlineSeq $ Symbol (T.pack o) <| i :| is
  where
    pOpen t = Parsec.string t <* pPutAcceptConstrained OpenAndClose
    pClose t = Parsec.string t <* pPutAcceptConstrained OpenAndClose
    pPush :: Parser Scope
    pPush = do
      scope@(Scope open _ _ _) <- pAnd $ do
        found <-
          choice
            $ fmap (\scope@(Scope open _ _ _) -> scope <$ Parsec.string open)
            $ defaultConstrainedScopes
        -- Check: not followed by space or eof
        -- TODO: Improve space check, guarantee that there is ~pWord afterwards
        pNot (pure () <$> pSpace <|> Parsec.eof)
        pure found
      -- Check: nesting rule
      -- TODO: Improve nesting rule
      Parsec.getState >>= \s -> case scopes s of
        (Scope openTop _ _ _ : _) | openTop == open -> empty
        _ -> pure ()
      -- Modify state, but not acceptedConstrained, as we don't consume input
      Parsec.modifyState $ \s -> s {scopes = scope : scopes s}
      pure scope
    pCheckScope :: Scope -> Parser Scope
    pCheckScope scope@(Scope _open close acceptability content) = do
      Parsec.string close
      -- Check: not followed by word
      -- TODO: improve word check. Only if Constrained needs a word afterwards
      pNot pWord
      pure scope
    pPull, pPull' :: Parser ScopeEnding
    pPull =
      pAnd (Parsec.eof) *> pure Interrupted
        <|> pPull'
    pPull' = do
      s <- Parsec.getState
      found <- pAnd $ choice $ fmap pCheckScope $ scopes s
      case scopes s of
        [] -> empty
        top : following -> do
          Parsec.putState $ s {scopes = following }
          case top == found of
            True -> pure Closed
            False -> pure Interrupted

pFallback :: Parser Inline
pFallback =
  Symbol . T.singleton <$> Parsec.anyChar

-- | Models PEG's @&@ operator using Parsec's 'Parsec.lookAhead' and
-- 'Parsec.try'.
pAnd :: Parser a -> Parser a
pAnd p = Parsec.lookAhead (Parsec.try p)

-- | Models PEG's @!@ operator.
--
-- Similar to 'Parsec.notFollowedBy', but @pNot p@ behaves as expected if @p@
-- does not consume input.
--
-- Probably inefficient.
pNot ::
  (Parsec.Stream s m t, Show a) =>
  Parsec.ParsecT s u m a ->
  Parsec.ParsecT s u m ()
pNot p =
  Parsec.try $ join $
    do a <- Parsec.try p; return (Parsec.unexpected (show a))
      <|> return (return ())

parseTest :: Parser a -> Text -> Either Parsec.ParseError a
parseTest parser text =
  Parsec.runParser parser initialState "" text
