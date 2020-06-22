module Text.Asciidoc.Inlines
  ( parseTestInline,
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
import Data.Char
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (Parsec, notFollowedBy, parseTest)
import Text.Parsec.Char (char, oneOf, satisfy, string)

type Parser = Parsec Text ()

data Inline
  = LineBreak
  | SoftBreak
  | Space
  | Str Text
  | Strong Inlines
  deriving (Eq, Show)

type Inlines = [Inline]

pInline :: Parser Inline
pInline = pure $ Str "Hello World!"

-- Parsing rules copied from
-- https://github.com/Mogztter/asciidoctor-inline-parser/blob/master/lib/asciidoctor/inline_parser/asciidoctor_grammar.treetop.

--  rule text
--    (quoted)* <Text>
--  end

--  rule quoted
--    (
--      escaped_passthrough_inline_macro / escaped_quoted_symbol / escaped_role_symbol / escaped_explicit_email / escaped_implicit_email /
--      image /
--      kbd / btn / menu /
--      passthrough_inline_macro / passthrough_triple_plus /
--      double_curved_quotes / single_curved_quotes /
--      literal /
--      explicit_link / explicit_link_protected / implicit_link /
--      explicit_email / implicit_email /
--      unconstrained_strong / unconstrained_emphasis / unconstrained_monospaced / unconstrained_mark /
--      strong / emphasis / monospaced / mark / superscript / subscript /
--      ' ' / word / symbol
--    )+ <Expression>
--  end

--  rule double_curved_quotes
--    '"`' double_curved_quotes_content '`"' <DoubleCurvedQuoted>
--  end

--  rule double_curved_quotes_content
--    [^`"]+ <QuotedContent>
--  end

--  rule single_curved_quotes
--    '\'`' single_curved_quotes_content '`\'' <SingleCurvedQuoted>
--  end

--  rule single_curved_quotes_content
--    [^`\']+ <QuotedContent>
--  end

--  rule literal
--    '+' (!spaces) literal_content (!spaces) '+' <Literal>
--  end

--  rule literal_content
--    [^+]+ <QuotedContent>
--  end

--  rule strong
--    quoted_text_attrs? '*' (!spaces) strong_content (!spaces) '*' (!constrained_mark_exception_end) <StrongQuoted>
--  end

--  rule unconstrained_strong
--    quoted_text_attrs? '**' strong_content '**' <StrongQuoted>
--  end

--  rule strong_content
--    (strong_content_greedy)+ <QuotedContent>
--  end

--  rule strong_content_greedy
--    ( [^*] / '*' constrained_mark_exception )
--  end

--  rule emphasis
--    quoted_text_attrs? '_' (!spaces) emphasis_content (!spaces) '_' (!constrained_mark_exception_end) <EmphasisQuoted>
--  end

--  rule unconstrained_emphasis
--    quoted_text_attrs? '__' emphasis_content '__' <EmphasisQuoted>
--  end

--  rule emphasis_content
--    (emphasis_content_greedy)+ <QuotedContent>
--  end

--  rule emphasis_content_greedy
--    ( [^_] / '_' constrained_mark_exception )
--  end

--  rule monospaced
--    quoted_text_attrs? '`' (!spaces) monospaced_content (!spaces) '`' (!constrained_mark_exception_end) <MonospacedQuoted>
--  end

--  rule unconstrained_monospaced
--    quoted_text_attrs? '``' monospaced_content '``' <MonospacedQuoted>
--  end

--  rule monospaced_content
--    (monospaced_content_greedy)+ <QuotedContent>
--  end

--  rule monospaced_content_greedy
--    ( [^`] / '`' constrained_mark_exception )
--  end

--  rule mark
--    quoted_text_attrs? '#' (!spaces) mark_content (!spaces) '#' (!constrained_mark_exception_end) <MarkQuoted>
--  end

--  rule unconstrained_mark
--    quoted_text_attrs? '##' mark_content '##' <MarkQuoted>
--  end

--  rule mark_content
--    (mark_content_greedy)+ <QuotedContent>
--  end

--  rule mark_content_greedy
--    ( [^#] / '#' constrained_mark_exception )
--  end

--  rule superscript
--    '^' (!spaces) superscript_content (!spaces) '^' <SuperscriptQuoted>
--  end

--  rule superscript_content
--    [^\^]+ <QuotedContent>
--  end

--  rule subscript
--    '~' (!spaces) subscript_content (!spaces) '~' <SubscriptQuoted>
--  end

--  rule subscript_content
--    [^~]+ <QuotedContent>
--  end

newtype SubscriptQuoted = SubscriptQuoted {unSubscriptQuoted :: Text}

pSubscript :: Parser (Text, SubscriptQuoted, Text)
pSubscript =
  (,,)
    <$> pQuoteChar <* notFollowedBy pSpace <*> pContent <*> pQuoteChar
  where
    pQuoteChar = T.singleton <$> char '~'
    -- FIXME: pContent should accept spaces and parse inner mark-up.
    pContent =
      SubscriptQuoted . T.pack . NE.toList
        <$> some (satisfy (\c -> c /= '~' && not (isAsciidocSpace c)))

--  rule quoted_text_attrs
--    '[' quoted_text_attrs_content ']' <QuotedTextAttributes>
--  end

--  rule quoted_text_attrs_content
--    ( quoted_text_anchor / quoted_text_role )* <QuotedTextAttributesContent>
--  end

data Attribute
  = Role RoleIdentifier
  | Anchor AnchorIdentifier

newtype QuotedTextAttributes = QuotedTextAttributes
  { unQuotedTextAttributes :: [Attribute]
  }

pQuotedTextAttributes :: Parser (Text, QuotedTextAttributes, Text)
pQuotedTextAttributes =
  (,,)
    <$> pOpen <*> p <*> pClose
  where
    p = QuotedTextAttributes <$> many pAttribute
    pOpen = T.singleton <$> char '['
    pClose = T.singleton <$> char ']'
    pAttribute =
      pRole
        <|> pAnchor
    pRole = Role . snd <$> pQuotedTextRole
    pAnchor = Anchor . snd <$> pQuotedTextAnchor

--  rule quoted_text_role
--    '.' role_identifier <QuotedTextRole>
--  end

--  rule quoted_text_anchor
--    '#' anchor_identifier <QuotedTextAnchor>
--  end

pQuotedTextRole :: Parser (Text, RoleIdentifier)
pQuotedTextRole =
  (,) <$> (T.singleton <$> char '.') <*> pRoleIdentifier

pQuotedTextAnchor :: Parser (Text, AnchorIdentifier)
pQuotedTextAnchor =
  (,) <$> (T.singleton <$> char '#') <*> pAnchorIdentifier

-- TODO: constrained_mark_exception
--
--  rule constrained_mark_exception_end
--    ( constrained_mark_exception )
--  end

--  rule constrained_mark_exception
--    '[\p{Word}&&[^_]]'r
--  end

--  rule escaped_quoted_symbol
--    ( '\*' / '\_' / '\`' / '\#' / '\^' / '\~' )
--  end

pEscapedQuotedSymbol :: Parser Text
pEscapedQuotedSymbol =
  (<>)
    <$> pBackslash <*> pQuotedSymbol
  where
    pBackslash = T.singleton <$> char '\\'
    pQuotedSymbol = T.singleton <$> oneOf (NE.toList quotedSymbols)

--  rule escaped_role_symbol
--    ( '\[' )
--  end

pEscapedRoleSymbol :: Parser Text
pEscapedRoleSymbol =
  T.pack
    <$> string "\\["

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
pBasicSymbol = oneOf $ NE.toList basicSymbols

basicSymbols :: NE.NonEmpty Char
basicSymbols =
  NE.fromList
    [ '&',
      '-',
      ':',
      ';',
      '=',
      ',',
      '"',
      '\'',
      '.',
      '!',
      '\\',
      '{',
      '}',
      ']',
      '[',
      '<',
      '>',
      '/',
      '(',
      ')'
    ]

pQuotedSymbols :: Parser Text
pQuotedSymbols =
  T.pack . NE.toList
    <$> some pQuotedSymbol
  where
    pQuotedSymbol = oneOf (NE.toList quotedSymbols)

quotedSymbols :: NE.NonEmpty Char
quotedSymbols =
  NE.fromList
    [ '_',
      '*',
      '#',
      '`',
      '^',
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
pIdentifierChar = satisfy isIdentifierChar
  where
    isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

--  rule word
--    alnum
--  end

--  rule number
--    [0-9]+
--  end

--  rule alpha
--    [a-zA-Z]+ # FIXME: should include all unicode characters ?
--  end

--  rule alnum
--    ( number / alpha )
--  end

-- NOTE: We include in pSpaces any space character that is not a newline.
--
--  rule spaces
--    [ ]+
--  end

-- | Like @spaces@, but with the following differences:
--
--     * It returns the parsed characters.
--
--     * Newlines are not considered space.
pSpaces :: Parser Text
pSpaces =
  T.pack . NE.toList
    <$> some pSpace

pSpace :: Parser Char
pSpace = satisfy isAsciidocSpace

isAsciidocSpace :: Char -> Bool
isAsciidocSpace c = isSpace c && c /= '\n'

parseTestInline :: Text -> IO ()
parseTestInline = parseTest pInline
