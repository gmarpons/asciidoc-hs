{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Text.AsciiDoc.Attributes
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of the Attribute Language for AsciiDoc, i.e., the language for
-- describing properties (styles, identifiers, roles, named attributes, etc.) of
-- entities like inlines and blocks.
--
-- This module contains Parsec-style parsers for the aforementioned language.
--
-- It tries to be compatible with Asciidoctor.
module Text.AsciiDoc.Attributes
  ( -- = Attribute Type
    Attribute (..),
    -- = Attribute Parsing
    AttributeParser,
    pAttributeList,
    pAttributeShorthandSyntax,
  )
where

import Control.Applicative.Permutations
import Control.Monad.Combinators hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators.NonEmpty
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.AsciiDoc.LineParsers as LP
import qualified Text.Parsec as Parsec

data Attribute
  = PositionalAttribute Text
  | NamedAttribute Text Text
  | -- | Special Asciidoctor syntax, all fields are optional and can appear in
    -- any order: @[style#id.role1.role2%option1%option2]@.
    --
    -- An empty style means no style has been specified.
    --
    -- Empty ids, roles and options are admissible, even if they normally rise a
    -- warning.
    --
    -- More than one id is admissible, even if this situation normally rises a
    -- warning.
    ShorthandSyntaxAttribute Text [Text] [Text] [Text]
  deriving (Eq, Show)

type AttributeParser = LP.LineParser

data Position
  = Start
  | Other

-- | Converts a @Text@ into a list of attributes.
--
-- Attributes are separated by commas.
--
-- Attributes can be empty (the empty string). Attributes can contain square
-- brackets.
--
-- Attributes can be enclosed between single or double quotes. Quote-enclosed
-- attributes can contain commas and quote characters (if escaped with "@\@").
--
-- This parser only fails on empty input or space as first character.
--
-- __Divergence from Asciidoctor__: When a string of the list is only partially
-- enclosed between single or double quotes, Asciidoctor breaks the string into
-- different attributes, and this function considers it a non-quoted single
-- attribute (i.e., quotes are part of the attribute and commas break the
-- string).
pAttributeList :: AttributeParser (NonEmpty Attribute)
pAttributeList =
  (:|)
    <$> pAttribute Start
    <*> (option [] (pSep *> sepBy (pAttribute Other) pSep)) <* Parsec.eof
  where
    pAttribute position =
      Parsec.try (pQuoted position '"' <* many Parsec.space <* pEnd)
        <|> Parsec.try (pQuoted position '\'' <* many Parsec.space <* pEnd)
        <|> pUnquoted position <* many Parsec.space <* pEnd
    pQuoted :: Position -> Char -> AttributeParser Attribute
    pQuoted position quote = do
      v <- pQuotedValue quote
      let shorthandOrError =
            Parsec.parse (pAttributeShorthandSyntax AcceptCommas <* Parsec.eof) "" v
      case (position, shorthandOrError) of
        -- First positional attribute, shorthand syntax accepted
        (Start, Right shorthand) -> pure shorthand
        -- First positional attribute, shorthand syntax failed: treat as a
        -- regular positional attribute
        (Start, Left _) -> pure $ PositionalAttribute v
        -- Position different from first: regular positional attribute
        (Other, _) -> pure $ PositionalAttribute v
    pUnquoted Start =
      Parsec.try pUnquotedShorthand
        <|> Parsec.try pNamed
        <|> pPositional
    pUnquoted Other =
      Parsec.try pNamed
        <|> pPositional
    pUnquotedShorthand = do
      shorthand <-
        -- Parsec.try not needed because this functions is only ever called
        -- inside a Parsec.try.
        pAttributeShorthandSyntax RejectCommas <* many Parsec.space <* pEnd
      case shorthand of
        -- No '=' found in the first segment of the potential shorthand syntax
        ShorthandSyntaxAttribute style _ _ _
          | T.all (/= '=') style -> pure shorthand
        _ -> empty
    pQuotedValue :: Char -> AttributeParser Text
    pQuotedValue quote =
      between (Parsec.char quote) (Parsec.char quote) $
        LP.manyText (LP.satisfy (\c -> c /= quote && c /= '\\'))
          <> LP.manyText
            ( (Parsec.try (LP.char '\\' *> LP.char quote) <|> LP.char '\\')
                <> LP.manyText (LP.satisfy (\c -> c /= quote && c /= '\\'))
            )
    pUnquotedValue :: AttributeParser Text
    pUnquotedValue = (T.strip . T.pack) <$> many (Parsec.satisfy (/= ','))
    pNamed :: AttributeParser Attribute
    pNamed =
      NamedAttribute
        <$> pName <* Parsec.char '=' <* many Parsec.space <*> pValue
    pName :: AttributeParser Text
    pName = LP.some (Parsec.noneOf [' ', ',', '=']) <* many Parsec.space
    pValue =
      Parsec.try (pQuotedValue '"')
        <|> Parsec.try (pQuotedValue '\'')
        <|> pUnquotedValue
    pPositional = PositionalAttribute <$> pUnquotedValue
    pSep = Parsec.char ',' <* many Parsec.space
    pEnd = Parsec.lookAhead $ eitherP (LP.char ',') Parsec.eof

data CommaAcceptance
  = AcceptCommas
  | RejectCommas
  deriving (Eq, Show)

pAttributeShorthandSyntax :: CommaAcceptance -> AttributeParser Attribute
pAttributeShorthandSyntax commaAcceptance =
  f <$> someTill pPermutation pEnd
  where
    -- Use monoid instances of (,), Text and [] to collect ids, roles and
    -- options.
    f ((s, (i, (r, o))) :| xs) =
      ShorthandSyntaxAttribute
        s
        (i <> (fst . snd . fold) xs)
        (r <> (fst . snd . snd . fold) xs)
        (o <> (snd . snd . snd . fold) xs)
    pPermutation :: AttributeParser (Text, ([Text], ([Text], [Text])))
    pPermutation =
      runPermutation $
        (\s i r o -> (s, (i, (r, o))))
          <$> toPermutationWithDefault "" pStyle
          <*> toPermutationWithDefault [] pIdentifier
          <*> toPermutationWithDefault [] pRole
          <*> toPermutationWithDefault [] pOption
    pStyle = LP.many anyChar
    pIdentifier = (: []) <$ Parsec.char '#' <*> LP.many anyChar
    pRole = (: []) <$ Parsec.char '.' <*> LP.many anyChar
    pOption = (: []) <$ Parsec.char '%' <*> LP.many anyChar
    anyChar = Parsec.noneOf $
      [' ', '#', '.', '%'] <> case commaAcceptance of
        AcceptCommas -> []
        RejectCommas -> [',']
    pEnd = Parsec.lookAhead $ eitherP Parsec.eof $ Parsec.oneOf [' ', ',']
