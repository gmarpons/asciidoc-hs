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
  ( -- * AST types
    Attribute (..),
    PositionedAttribute (..),

    -- * Parsers
    AttributeParser,
    attributeListP,
    attributeShorthandSyntaxP,
    CommaAcceptance (..),
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

newtype PositionedAttribute = PositionedAttribute (Int, Attribute)

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
attributeListP :: AttributeParser (NonEmpty Attribute)
attributeListP =
  (:|)
    <$> attributeP Start
    <*> option [] (sepP *> sepBy (attributeP Other) sepP) <* Parsec.eof
  where
    attributeP position =
      Parsec.try (quotedP position '"' <* many Parsec.space <* endP)
        <|> Parsec.try (quotedP position '\'' <* many Parsec.space <* endP)
        <|> unquotedP position <* many Parsec.space <* endP
    quotedP :: Position -> Char -> AttributeParser Attribute
    quotedP position quote = do
      v <- quotedValueP quote
      let shorthandOrError =
            Parsec.parse (attributeShorthandSyntaxP AcceptCommas <* Parsec.eof) "" v
      case (position, shorthandOrError) of
        -- First positional attribute, shorthand syntax accepted
        (Start, Right shorthand) -> pure shorthand
        -- First positional attribute, shorthand syntax failed: treat as a
        -- regular positional attribute
        (Start, Left _) -> pure $ PositionalAttribute v
        -- Position different from first: regular positional attribute
        (Other, _) -> pure $ PositionalAttribute v
    unquotedP Start =
      Parsec.try unquotedShorthandP
        <|> Parsec.try namedP
        <|> positionalP
    unquotedP Other =
      Parsec.try namedP
        <|> positionalP
    unquotedShorthandP = do
      shorthand <-
        -- Parsec.try not needed because this functions is only ever called
        -- inside a Parsec.try.
        attributeShorthandSyntaxP RejectCommas <* many Parsec.space <* endP
      case shorthand of
        -- No '=' found in the first segment of the potential shorthand syntax
        ShorthandSyntaxAttribute style _ _ _
          | T.all (/= '=') style -> pure shorthand
        _ -> empty
    quotedValueP :: Char -> AttributeParser Text
    quotedValueP quote =
      between (Parsec.char quote) (Parsec.char quote) $
        LP.manyText (LP.satisfy (\c -> c /= quote && c /= '\\'))
          <> LP.manyText
            ( (Parsec.try (LP.char '\\' *> LP.char quote) <|> LP.char '\\')
                <> LP.manyText (LP.satisfy (\c -> c /= quote && c /= '\\'))
            )
    unquotedValueP :: AttributeParser Text
    unquotedValueP = T.strip . T.pack <$> many (Parsec.satisfy (/= ','))
    namedP :: AttributeParser Attribute
    namedP =
      NamedAttribute
        <$> nameP <* Parsec.char '=' <* many Parsec.space <*> valueP
    nameP :: AttributeParser Text
    nameP = LP.some (Parsec.noneOf [' ', ',', '=']) <* many Parsec.space
    valueP =
      Parsec.try (quotedValueP '"')
        <|> Parsec.try (quotedValueP '\'')
        <|> unquotedValueP
    positionalP = PositionalAttribute <$> unquotedValueP
    sepP = Parsec.char ',' <* many Parsec.space
    endP = Parsec.lookAhead $ eitherP (LP.char ',') Parsec.eof

data CommaAcceptance
  = AcceptCommas
  | RejectCommas
  deriving (Eq, Show)

attributeShorthandSyntaxP :: CommaAcceptance -> AttributeParser Attribute
attributeShorthandSyntaxP commaAcceptance =
  wrap <$> someTill permutationP endP
  where
    -- Use monoid instances of (,), Text and [] to collect ids, roles and
    -- options.
    wrap ((s, (i, (r, o))) :| xs) =
      ShorthandSyntaxAttribute
        s
        (i <> (fst . snd . fold) xs)
        (r <> (fst . snd . snd . fold) xs)
        (o <> (snd . snd . snd . fold) xs)
    permutationP :: AttributeParser (Text, ([Text], ([Text], [Text])))
    permutationP =
      runPermutation $
        (\s i r o -> (s, (i, (r, o))))
          <$> toPermutationWithDefault "" styleP
          <*> toPermutationWithDefault [] identifierP
          <*> toPermutationWithDefault [] roleP
          <*> toPermutationWithDefault [] optionP
    styleP = LP.many anyChar
    identifierP = (: []) <$ Parsec.char '#' <*> LP.many anyChar
    roleP = (: []) <$ Parsec.char '.' <*> LP.many anyChar
    optionP = (: []) <$ Parsec.char '%' <*> LP.many anyChar
    anyChar =
      Parsec.noneOf $
        [' ', '#', '.', '%'] <> case commaAcceptance of
          AcceptCommas -> []
          RejectCommas -> [',']
    endP = Parsec.lookAhead $ eitherP Parsec.eof $ Parsec.oneOf [' ', ',']
