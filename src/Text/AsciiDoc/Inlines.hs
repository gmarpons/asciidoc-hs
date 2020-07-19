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
    Inlines,
    QuoteType (..),
    pInlines,
    parseTestInlines,
  )
where

import Control.Monad.Combinators hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators.NonEmpty
import Data.Bifunctor
import Data.Char hiding (Space)
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
  ( Parsec,
    eof,
    notFollowedBy,
    parseTest,
  )
import qualified Text.Parsec.Char as Parsec
  ( char,
    oneOf,
    satisfy,
    string,
  )

type Parser = Parsec.Parsec Text ()

data QuoteType
  = Bold
  | Italic
  | Monospace
  deriving (Eq, Show)

data Inline
  = Space
  | Word Text
  | Symbol Text
  | Quote QuoteType Inlines
  deriving (Eq, Show)

type Inlines = [Inline]

pInlines :: Parser (NonEmpty Inline)
pInlines =
  postProcessQuote <$> pTentativeQuote <*> pPostQuote
    <|> (:|) <$> pWord <*> pPostWord
  where
    pInlines' = NE.toList <$> pInlines
    pPostQuote :: Parser Inlines
    pPostQuote =
      pInlines'
        <|> [] <$ Parsec.eof
    pPostWord :: Parser Inlines
    pPostWord =
      (:) <$> pSpaces' <*> pPostPostWord
        <|> (:) <$> pDelimiter' <*> pPostDelimiter
        <|> [] <$ Parsec.eof
    pPostPostWord =
      pInlines'
        <|> [] <$ Parsec.eof
    pSpaces' = Space <$ pSpaces
    pDelimiter' = Symbol . T.singleton <$> pDelimiter
    pPostDelimiter =
      (:) <$> pSpaces' <*> pInlines'
        <|> (:) <$> pWord <*> pPostWord

postProcessQuote ::
  Either (NonEmpty Inline) (NonEmpty Inline) ->
  Inlines ->
  NE.NonEmpty Inline
postProcessQuote (Left (t :| ts)) us = t :| ts <> us
postProcessQuote (Right (Symbol "*" :| ts)) us = removeDelim (reverse ts)
  where
    removeDelim (Symbol "*" : ts') = Quote Bold (reverse ts') :| us
    removeDelim (Space : Symbol "*" : ts') = Quote Bold (reverse ts') <| Space :| us
    removeDelim _ = error "postProcessQuote: unexpected Strong ending"
postProcessQuote _ _ = error "postProcessQuote: unexpected Strong beginning"

pTentativeQuote :: Parser (Either (NonEmpty Inline) (NonEmpty Inline))
pTentativeQuote =
  (\t -> bimap (t :|) (t :|))
    <$> pDelimiter' <* Parsec.notFollowedBy pDelimiter <*> pPostQuoteOpening
  where
    pPostQuoteOpening =
      Left [Space] <$ pSpaces
        <|> pQuoteContinuation
    pDelimiter' = Symbol . T.singleton <$> pDelimiter

pQuoteContinuation :: Parser (Either Inlines Inlines)
pQuoteContinuation =
  (\t -> bimap (t :) (t :)) <$> pWord <*> pQuoteContinuation'
    <|> (\t -> bimap (t :) (t :))
      <$> pDelimiter' <* Parsec.notFollowedBy pDelimiter <*> pQuoteContinuation''
    <|> Left [] <$ Parsec.eof
  where
    pQuoteContinuation' :: Parser (Either Inlines Inlines)
    pQuoteContinuation' =
      (\t -> bimap (t :) (t :)) <$> pDelimiter' <*> pPostDelim
        <|> (\t -> bimap (t :) (t :)) <$> pSpaces' <*> pQuoteContinuation
        <|> Left [] <$ Parsec.eof
    pQuoteContinuation'' =
      (\mt -> maybe id (\t -> bimap (t :) (t :)) mt)
        <$> optional pSpaces' <*> pQuoteContinuation
    pSpaces' = Space <$ pSpaces
    pDelimiter' = Symbol . T.singleton <$> pDelimiter
    pPostDelim :: Parser (Either Inlines Inlines)
    pPostDelim =
      Right [Space] <$ pSpaces -- TODO: to simplify post processing, use try and return Right []
        <|> Right [] <$ Parsec.eof
        <|> pQuoteContinuation

-- An example delimiter. TODO: generalize to any quote delimiter.
pDelimiter :: Parser Char
pDelimiter = Parsec.char '*'

pWord :: Parser Inline
pWord =
  Word . T.pack . NE.toList
    <$> some pWordChar
  where
    pWordChar = Parsec.satisfy $ \c ->
      isAlphaNum c

-- We try to follow rules in
-- https://github.com/Mogztter/asciidoctor-inline-parser/blob/master/lib/asciidoctor/inline_parser/asciidoctor_grammar.treetop
-- in some of the following definitions.
data Attribute
  = Role RoleIdentifier
  | Anchor AnchorIdentifier

newtype QuotedTextAttributes = QuotedTextAttributes
  { unQuotedTextAttributes :: [Attribute]
  }

pQuotedTextAttributes :: Parser QuotedTextAttributes
pQuotedTextAttributes =
  pOpen *> p <* pClose
  where
    p = QuotedTextAttributes <$> many pAttribute
    pOpen = T.singleton <$> Parsec.char '['
    pClose = T.singleton <$> Parsec.char ']'
    pAttribute =
      pRole
        <|> pAnchor
    pRole = Role <$> pQuotedTextRole
    pAnchor = Anchor <$> pQuotedTextAnchor

--  rule quoted_text_role
--    '.' role_identifier <QuotedTextRole>
--  end

--  rule quoted_text_anchor
--    '#' anchor_identifier <QuotedTextAnchor>
--  end

pQuotedTextRole :: Parser RoleIdentifier
pQuotedTextRole =
  Parsec.char '.' *> pRoleIdentifier

pQuotedTextAnchor :: Parser AnchorIdentifier
pQuotedTextAnchor =
  Parsec.char '#' *> pAnchorIdentifier

-- NOTE: We include '_' (Low line, U+005F) as part of
-- pCharThatCannotFollowConstrainedQuote

--  rule constrained_mark_exception_end
--    ( constrained_mark_exception )
--  end

--  rule constrained_mark_exception
--    '[\p{Word}&&[^_]]'r
--  end

pCharThatCannotFollowConstrainedQuote :: Parser Char
pCharThatCannotFollowConstrainedQuote =
  Parsec.satisfy $ \c ->
    isAlphaNum c || generalCategory c
      `elem` [ ConnectorPunctuation,
               SpacingCombiningMark,
               EnclosingMark
             ]

--  rule escaped_quoted_symbol
--    ( '\*' / '\_' / '\`' / '\#' / '\^' / '\~' )
--  end

pEscapedQuotedSymbol :: Parser Text
pEscapedQuotedSymbol =
  (<>)
    <$> pBackslash <*> pQuotedSymbol
  where
    pBackslash = T.singleton <$> Parsec.char '\\'
    pQuotedSymbol = T.singleton <$> Parsec.oneOf (NE.toList quotedSymbols)

--  rule escaped_role_symbol
--    ( '\[' )
--  end

pEscapedRoleSymbol :: Parser Text
pEscapedRoleSymbol =
  T.pack
    <$> Parsec.string "\\["

--  rule symbol
--    ( symbol_basic / quoted_symbol )
--  end

--  rule symbol_basic
--    [&\-:;=,"'\.!\\{}\]\[<>/()]
--  end

--  rule quoted_symbol
--    [_*#`^~]+
--  end

pSymbol :: Parser Text
pSymbol =
  (T.singleton <$> pBasicSymbol)
    <|> pQuotedSymbols

pBasicSymbol :: Parser Char
pBasicSymbol = Parsec.oneOf $ NE.toList basicSymbols

basicSymbols :: NE.NonEmpty Char
basicSymbols =
  '!'
    :| [ '"',
         '&',
         '\'',
         '(',
         ')',
         ',',
         '-',
         '.',
         '/',
         ':',
         ';',
         '<',
         '=',
         '>',
         '[',
         '\\',
         ']',
         '{',
         '}'
       ]

pQuotedSymbols :: Parser Text
pQuotedSymbols =
  T.pack . NE.toList
    <$> some pQuotedSymbol
  where
    pQuotedSymbol = Parsec.oneOf (NE.toList quotedSymbols)

quotedSymbols :: NE.NonEmpty Char
quotedSymbols =
  '#'
    :| [ '*',
         '^',
         '_',
         '`',
         '~'
       ]

--  rule role_identifier
--    identifier+ <RoleIdentifier>
--  end

--  rule anchor_identifier
--    identifier+ <AnchorIdentifier>
--  end

newtype RoleIdentifier = RoleIdentifier {unRoleIdentifier :: Text}

newtype AnchorIdentifier = AnchorIdentifier {unAnchorIdentifier :: Text}

pRoleIdentifier :: Parser RoleIdentifier
pRoleIdentifier =
  RoleIdentifier . T.pack . NE.toList
    <$> some pIdentifierChar

pAnchorIdentifier :: Parser AnchorIdentifier
pAnchorIdentifier =
  AnchorIdentifier . T.pack . NE.toList
    <$> some pIdentifierChar

--  rule identifier
--    [0-9a-zA-Z_\-]
--  end

pIdentifierChar :: Parser Char
pIdentifierChar = Parsec.satisfy isIdentifierChar
  where
    isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

-- | Like @Text.Parsec.Char.spaces@, but with the following differences:
--
--     * It returns the parsed characters.
--
--     * Newlines are not considered space.
--
-- It's also different from rules found in
-- https://github.com/Mogztter/asciidoctor-inline-parser/blob/master/lib/asciidoctor/inline_parser/asciidoctor_grammar.treetop
-- in that we include in @pSpaces@ any space character that is not a newline.
pSpaces :: Parser Text
pSpaces =
  T.pack . NE.toList
    <$> some pSpace

pSpace :: Parser Char
pSpace = Parsec.satisfy isAsciiDocSpace

isAsciiDocSpace :: Char -> Bool
isAsciiDocSpace c = isSpace c && c /= '\n'

parseTestInlines :: String -> Text -> IO ()
parseTestInlines label text = do
  putStrLn "=========="
  putStrLn $ label <> ":"
  Parsec.parseTest pInlines text
  putStrLn "=========="
  putStrLn ""
