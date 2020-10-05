-- |
-- Module      :  Text.AsciiDoc.LineParsers
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parsec-style parser combinators to help classifying individual AsciiDoc
-- lines.
--
-- All parsers in this module return 'Data.Text.Text', which helps to combine
-- them using the @Monoid@ instance of 'Text.Parsec.ParsecT'.
module Text.AsciiDoc.LineParsers
  ( -- * Parser type
    LineParser,

    -- * Helper parser combinators
    blockId,
    blockAttributeList,
    runOfN,
    anyRemainder,
    many,
    some,
    count,
    string,
    satisfy,
  )
where

import Control.Monad (MonadPlus)
import qualified Control.Monad.Combinators as PC hiding
  ( endBy1,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
  )
import Control.Monad.Combinators ((<|>))
import qualified Control.Monad.Combinators.NonEmpty as PC
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.Functor.Identity (Identity)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec

-- | Parser type used to check syntactic conditions on single lines of an
-- AsciiDoc document.
type LineParser = Parsec.ParsecT Text () Identity

-- | Accepts a block identifier: an alphanumeric string starting with a letter
-- (or "@_@") surrounded by "@[[@" and "@]]@".
--
-- Accepts null identifiers (the empty string).
--
-- It does not accept spaces between the square brackets.
blockId :: LineParser Text
blockId =
  string "[["
    *> PC.option T.empty (satisfy isBlockIdFirstChar <> many Parsec.alphaNum)
      <* string "]]"
  where
    isBlockIdFirstChar c = c `elem` ['_', 'º', 'ª'] || isLetter c

-- | Accepts an square-bracket-enclosed list of strings separated by commas.
-- Every string is considered an attribute.
--
-- Attributes can be empty (the empty string). Attributes can contain square
-- brackets.
--
-- Attributes can be enclosed between single or double quotes. Quote-enclosed
-- attributes can contain commas and quote characters (if escaped with "@\@").
--
-- __Divergence from Asciidoctor__: When a string of the list is only partially
-- enclosed between single or double quotes, Asciidoctor breaks the string into
-- different attributes, and Asciidoc-hs considers it a non-quoted single
-- attribute (i.e., quotes are part of the attribute and commas break the
-- string).
blockAttributeList :: LineParser [Text]
blockAttributeList = do
  t <- string "[" *> anyRemainder
  let (t', remainder) = T.breakOnEnd "]" t
  case (T.all isSpace remainder, T.unsnoc t') of
    -- No char between square brackets: empty attribute list
    (True, Just ("", ']')) -> pure []
    -- There are some chars between square brackets: accept if first char
    -- belongs to firstChar
    (True, Just (t'', ']')) -> do
      case Parsec.parse pAttributeList "" t'' of
        Right attributeList -> pure $ attributeList
        Left _ -> PC.empty
    -- No closing bracket found at the end of the line: fail
    _ -> PC.empty
  where
    pAttributeList =
      (:) <$> pFirst <*> (PC.option [] (pSep *> PC.sepBy pFollowing pSep)) <* Parsec.eof
    pFirst =
      Parsec.try (pQuotedAttribute '"' <* many Parsec.space <* pEnd)
        <|> Parsec.try (pQuotedAttribute '\'' <* many Parsec.space <* pEnd)
        <|> pFirstChar <> manyText (satisfy (/= ','))
    -- If no quotes, restrict first char to a restricted number of characters,
    -- discovered empirically.
    pFirstChar = satisfy $
      \c -> isAlphaNum c || c `elem` [',', '.', '#', '%', '_', 'º', 'ª', '\'', '"']
    pFollowing =
      Parsec.try (pQuotedAttribute '"' <* many Parsec.space <* pEnd)
        <|> Parsec.try (pQuotedAttribute '\'' <* many Parsec.space <* pEnd)
        <|> manyText (satisfy (/= ','))
    pQuotedAttribute quote =
      PC.between (Parsec.char quote) (Parsec.char quote) (pAttribute quote)
    pAttribute quote =
      manyText (satisfy (\c -> c /= quote && c /= '\\'))
        <> manyText
          ( (Parsec.try (char '\\' *> char quote) <|> char '\\')
              <> manyText (satisfy (\c -> c /= quote && c /= '\\'))
          )
    pSep = Parsec.char ',' <* many Parsec.space
    pEnd = Parsec.lookAhead $ PC.eitherP (char ',') (Parsec.eof)

-- | @runOfN n cs@ creates a list of parsers, one for every character @c@ member
-- of @cs@. Each of these parsers accepts any run of @n@ or more consecutive
-- appearances of @c@.
--
-- Example: @runOfN 4 ['+', '=']@ accepts runs of four or more symbols @"+"@, or
-- four or more symbols @"="@.
runOfN :: Int -> [Char] -> [LineParser Text]
runOfN n = fmap $ \c -> count n (Parsec.char c) <> many (Parsec.char c)

-- | Returns (parses successfully) the remaining text of the line, whatever its
-- content.
anyRemainder :: LineParser Text
anyRemainder = Parsec.getInput <* Parsec.setInput ""

many :: MonadPlus f => f Char -> f Text
many p = T.pack <$> PC.many p

manyText :: MonadPlus f => f Text -> f Text
manyText p = T.concat <$> PC.many p

some :: MonadPlus f => f Char -> f Text
some p = (T.pack . NE.toList) <$> PC.some p

count :: Monad f => Int -> f Char -> f Text
count m p = T.pack <$> PC.count m p

string :: String -> LineParser Text
string s = T.pack <$> Parsec.string s

char :: Char -> LineParser Text
char c = T.singleton <$> Parsec.char c

satisfy :: (Char -> Bool) -> LineParser Text
satisfy f = T.singleton <$> Parsec.satisfy f
