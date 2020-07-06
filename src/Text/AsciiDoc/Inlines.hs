{-# LANGUAGE LambdaCase #-}

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
    pInlines,
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
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
  ( Parsec,
    ParsecT,
    Stream,
    eof,
    lookAhead,
    try,
    unexpected,
  )
import qualified Text.Parsec.Char as Parsec
  ( alphaNum,
    char,
    digit,
    noneOf,
    oneOf,
    string,
  )

type Parser = Parsec.Parsec Text ()

data Inline
  = Space
  | Word Text
  | Bold (NonEmpty Inline)
  | Symbol Char
  | Fallback Text
  deriving (Eq, Show)

type Inlines = [Inline]

pInlines :: Parser (NonEmpty Inline)
pInlines = some pInlineElement

pSpaces :: Parser Text
pSpaces = T.pack . NE.toList <$> some pSpace

-- | Models PEG's @&@ operator using Parsec's 'Parsec.lookAhead' and
-- 'Parsec.try'.
pAnd :: Parser a -> Parser ()
pAnd p = () <$ Parsec.lookAhead (Parsec.try p)

-- | Models PEG's @!@ operator.
--
-- Similar to 'Parsec.notFollowedBy', but @pNot p@ behaves as expected if @p@
-- does not consume input.
--
-- Probably inefficient.
pNot :: (Parsec.Stream s m t, Show a) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m ()
pNot p =
  Parsec.try $ join $
    do a <- Parsec.try p; return (Parsec.unexpected (show a))
      <|> return (return ())

-- | PEG-style ordered choice, with automatic backtracking.
(</>) :: Parser a -> Parser a -> Parser a
p </> q = Parsec.try p <|> q

infixl 3 </>

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1051
--
-- InlineElement <-
--     element:(InlineWord // more permissive than words
--         / LineBreak // must be before spaces
--         / Space+
--         / !EOL (
--             QuotedString
--             / QuotedText
--             / Symbol
--             / InlineIcon
--             / InlineImage
--             / Link
--             / InlinePassthrough
--             / InlineFootnote
--             / CrossReference
--             / InlineUserMacro
--             / AttributeSubstitution
--             / InlineElementID
--             / ConcealedIndexTerm
--             / IndexTerm
--             / ImpliedApostrophe
--             / AnyChar)) {
--     return element, nil
-- }

pInlineElement :: Parser Inline
pInlineElement =
  Word <$> pAlphaNums -- TODO: pInlineWord in PEG rule above
    </> Space <$ some pSpace
    </> pNot pEol *> pInlineElement'
  where
    pInlineElement' =
      pBoldText
        -- NOTE. Special case, not present in PEG above, to possibly treat some
        -- characters in a different way of general fallback.
        </> Fallback . T.singleton
          <$> Parsec.oneOf (NE.toList constrainedQuotedTextMarkers)
        </> Fallback . T.singleton <$> pAnyChar

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1129
--
-- BoldText <- DoubleQuoteBoldText / SingleQuoteBoldText // double punctuation must be evaluated first

pBoldText :: Parser Inline
pBoldText =
  pDoubleQuoteBoldText
    </> pSingleQuoteBoldText

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1131
--
-- DoubleQuoteBoldText <- attrs:(QuotedTextAttrs)? "**" elements:(DoubleQuoteBoldTextElements) "**" {
--     return types.NewQuotedText(types.Bold, attrs, elements.([]interface{}))
-- }

pDoubleQuoteBoldText :: Parser Inline
pDoubleQuoteBoldText =
  -- TODO: optional pQuotedTextAttrs
  Bold
    <$ pDelimiter <*> pDoubleQuoteBoldTextElements <* pDelimiter
  where
    pDelimiter = () <$ Parsec.string "**"

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1135
--
-- DoubleQuoteBoldTextElements <- DoubleQuoteBoldTextElement (!("**") (Space / DoubleQuoteBoldTextElement))*  // may start and end with spaces

pDoubleQuoteBoldTextElements :: Parser (NonEmpty Inline)
pDoubleQuoteBoldTextElements =
  (:|)
    -- NOTE. Diverges from above PEG rule: spaces are parsed as such at the
    -- beginning of the inline, and not as fallback characters.
    <$> q <*> many p
  where
    p = pNot pDelimiter *> q
    q =
      -- NOTE. diverges from above PEG rule: multiple spaces can be parsed as
      -- one single inline.
      Space <$ pSpaces
        <|> pDoubleQuoteBoldTextElement
    pDelimiter = () <$ Parsec.string "**"

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1137
--
-- DoubleQuoteBoldTextElement <- Word
--         / Newline !Newline
--         / SingleQuoteBoldText
--         / QuotedString
--         / ItalicText
--         / MarkedText
--         / MonospaceText
--         / SubscriptText
--         / SuperscriptText
--         / Symbol
--         / InlineIcon
--         / InlineImage
--         / Link
--         / InlinePassthrough
--         / AttributeSubstitution
--         / ImpliedApostrophe
--         / DoubleQuoteBoldTextFallbackCharacter

pDoubleQuoteBoldTextElement :: Parser Inline
pDoubleQuoteBoldTextElement =
  Word <$> pAlphaNums -- TODO: pWord in PEG rule above
    </> pSingleQuoteBoldText
    </> pDoubleQuoteBoldTextFallbackCharacter

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1156
--
-- DoubleQuoteBoldTextFallbackCharacter <-
--     [^\r\n*] // anything except EOL and bold delimiter (fallback in case nothing else matched)
--     / "**" Alphanums {  // or a bold delimiter when immediately followed by an alphanum (ie, in the middle of some text)
--     return types.NewStringElement(string(c.text))
-- }

pDoubleQuoteBoldTextFallbackCharacter :: Parser Inline
pDoubleQuoteBoldTextFallbackCharacter =
  Fallback <$> p
  where
    p =
      T.singleton <$> Parsec.noneOf "\r\n*"
        <|> (<>) <$> pDelimiter <*> pAlphaNums
    pDelimiter = T.pack <$> Parsec.string "**"

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1162
--
-- SingleQuoteBoldText <- attrs:(QuotedTextAttrs)? ( "*" !"*") elements:(SingleQuoteBoldTextElements) "*" &(!Alphanum) { // single punctuation cannot be followed by a character (needs '**' to emphazise a portion of a word)
--     return types.NewQuotedText(types.Bold, attrs, elements.([]interface{}))
-- } / attrs:(QuotedTextAttrs)? "*" elements:("*" SingleQuoteBoldTextElements) "*" { // unbalanced `**` vs `*` punctuation.
--     return types.NewQuotedText(types.Bold, attrs, elements.([]interface{})) // include the second heading `*` as a regular StringElement in the bold content
-- }

pSingleQuoteBoldText :: Parser Inline
pSingleQuoteBoldText =
  -- TODO: optional pQuotedTextAttrs
  Bold <$ (pDelimiter <* pNot pDelimiter) <*> pSingleQuoteBoldTextElements <* p
    -- NOTE. Diverges from PEG rule above: we add 'p' to the end of the
    -- following case.
    </> Bold <$ pDelimiter <*> q <* p
  where
    p = pDelimiter <* pAnd (pNot pAlphaNums)
    q = (<|) <$> pDelimiter' <*> pSingleQuoteBoldTextElements
    pDelimiter = () <$ Parsec.char '*'
    pDelimiter' = Fallback . T.singleton <$> Parsec.char '*'

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1168
--
-- SingleQuoteBoldTextElements <- !Space SingleQuoteBoldTextElement+

pSingleQuoteBoldTextElements :: Parser (NonEmpty Inline)
pSingleQuoteBoldTextElements =
  (:|) <$ pNot pSpace <*> pSingleQuoteBoldTextElement <*> p
  where
    p = concat <$> many p'
    p' =
      r
        </> (:) <$> pSpaces' <*> q
    q = maybe [] (: []) <$> optional (pDelimiter <* pNot pDelimiter) -- TODO: maybe
    r = (: []) <$ pNot pSpace <*> pSingleQuoteBoldTextElement
    pSpaces' = Space <$ pSpaces
    pDelimiter = Fallback . T.singleton <$> Parsec.char '*'

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1170
--
-- SingleQuoteBoldTextElement <- Word
--         / Newline !Newline
--         / DoubleQuoteBoldText
--         / QuotedString
--         / Space+ ('*' !'*')?
--         / ItalicText
--         / MarkedText
--         / MonospaceText
--         / SubscriptText
--         / SuperscriptText
--         / Symbol
--         / InlineIcon
--         / InlineImage
--         / Link
--         / InlinePassthrough
--         / AttributeSubstitution
--         / ImpliedApostrophe
--         / SingleQuoteBoldTextFallbackCharacter

pSingleQuoteBoldTextElement :: Parser Inline
pSingleQuoteBoldTextElement =
  Word <$> pAlphaNums -- TODO: pWord in PEG rule above
    </> pDoubleQuoteBoldText
    -- NOTE. Diverges from PEG rule above: missing disjunction of spaces
    -- followed by '*' because we've handled this case in
    -- pSingleQuoteBoldTextElements.
    </> pSingleQuoteBoldTextFallbackCharacter

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L1189
--
-- SingleQuoteBoldTextFallbackCharacter <-
--     [^\r\n*] // anything except EOL and bold delimiter (fallback in case nothing else matched)
--     / "*" Alphanums {  // or a bold delimiter when immediately followed by an alphanum (ie, in the middle of some text)
--     return types.NewStringElement(string(c.text))
-- }

pSingleQuoteBoldTextFallbackCharacter :: Parser Inline
pSingleQuoteBoldTextFallbackCharacter =
  Fallback <$> p
  where
    p =
      T.singleton <$> Parsec.noneOf "\r\n*"
        <|> (<>) <$> pDelimiter <*> pAlphaNums
    pDelimiter = T.pack <$> Parsec.string "*"

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2059
--
-- Parenthesis <- "(" / ")" / "[" / "]" / "{" / "}"

pParenthesis :: Parser Char
pParenthesis = Parsec.oneOf ['(', ')', '[', ']', '{', '}']

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2061
--
-- Alphanums <- [\pL0-9]+ {
--     return string(c.text), nil
-- }

pAlphaNums :: Parser Text
pAlphaNums = T.pack . NE.toList <$> some Parsec.alphaNum

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2065
--
-- Word <-
--     // very straightforward content: alphanums followed by attached simple quote delimiter and more characters
--     // (in this case, the quoted text delimiters are intepreted as regular characters)
--     // then followed by spaces but not the "+" signs because it needs a heading space to become a LineBreak element
--     [\pL0-9]+ &([\r\n ,\]] / EOF) {
--         return types.NewStringElement(string(c.text))
--     } / [\pL0-9]+ ([=*_`] [\pL0-9]+)+ {
--         return types.NewStringElement(string(c.text))
--     }

pWord :: Parser Text
pWord =
  pAlphaNums <* pAnd pDelimiter -- TODO: (Performance) factor pWord
    </> (<>) <$> pAlphaNums <*> p'
  where
    p' = T.cons <$> Parsec.oneOf specialChars <*> pAlphaNums
    pDelimiter =
      () <$ Parsec.oneOf "\r\n ,]" -- TODO: check if Unicode spaces are relevant
        <|> pEof
    specialChars = '=' : (NE.toList coreConstrainedQuotedTextMarkers)

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2075
--
-- InlineWord <- // same as word, but may also contain some punctuation markers
--     [\pL0-9,?!;]+ &([\r\n ] / EOF) {
--         return types.NewStringElement(string(c.text))
--     } / Word

pInlineWord :: Parser Text
pInlineWord =
  T.pack . NE.toList
    <$> some p <* pAnd pDelimiter
    </> pWord -- TODO: (Performance) factor pInlineWord
  where
    p =
      Parsec.alphaNum
        <|> Parsec.oneOf ",?!;" -- TODO: check if other punctuation can be accepted
    pDelimiter =
      () <$ Parsec.oneOf "\r\n " -- TODO: check if Unicode spaces are relevant
        <|> pEof

constrainedQuotedTextMarkers :: NE.NonEmpty Char
constrainedQuotedTextMarkers =
  '#'
    :| [ '*',
         '^',
         '_',
         '`',
         '~'
       ]

coreConstrainedQuotedTextMarkers :: NE.NonEmpty Char
coreConstrainedQuotedTextMarkers =
  '*'
    :| [ '_',
         '`'
       ]

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2080
--
-- // this is a fall-back rule in case all preceeding rules failed to match the current content.
-- AnyChar <- [^\r\n] {
--     return types.NewStringElement(string(c.text))
-- }

pAnyChar :: Parser Char
pAnyChar = Parsec.noneOf ['\r', '\n']

-- DIGIT <- [0-9] {
--     return string(c.text), nil
-- }

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2120
--

-- | Parses an ASCII digit.
pDigit :: Parser Char
pDigit = Parsec.digit

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2124
--
-- NUMBER <- "-"? DIGIT+ {
--     return strconv.Atoi(string(c.text))
-- }

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2128
--
-- Space <- " " / "\t" {
--     return string(c.text), nil
-- }

-- TODO: Check if unicode spaces are relevant.
pSpace :: Parser Char
pSpace = Parsec.oneOf [' ', '\t']

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2132
--
-- Newline <- "\r\n" / "\r" / "\n"

pNewline :: Parser Text
pNewline =
  T.pack <$> Parsec.string "\n"
    <|> T.cons <$> Parsec.char '\r' <*> pOptionalNewlineChar
  where
    pOptionalNewlineChar = maybe "" T.singleton <$> optional (Parsec.char '\n')

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2134
--
-- EOF <- !.

pEof :: Parser ()
pEof = Parsec.eof

-- https://github.com/bytesparadise/libasciidoc/blob/04f2a1d9e3e16deb9bf3630184190cd2ea51557b/pkg/parser/parser.peg#L2136
--
-- EOL <- Newline / EOF

pEol :: Parser Text
pEol =
  pNewline
    <|> T.empty <$ pEof
