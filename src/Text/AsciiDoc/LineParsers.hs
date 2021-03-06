{-# LANGUAGE OverloadedStrings #-}

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
  ( -- * Line parser type
    LineParser,

    -- * Concrete parsers
    blockId,
    blockAttributeList,

    -- * Generic parsers accepting 'Marker's
    runOfN,
    count,

    -- * Generic parsers accepting 'Text'
    anyRemainder,
    many,
    manyText,
    some,
    string,
    char,
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
import qualified Control.Monad.Combinators.NonEmpty as PC
import Data.Char (isAlphaNum, isDigit, isLetter, isSpace, ord)
import Data.Functor.Identity (Identity)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Text.AsciiDoc.SpecialChars
import qualified Text.Parsec as Parsec

-- | Parser type used to check syntactic conditions on single lines of an
-- AsciiDoc document.
type LineParser = Parsec.ParsecT Text () Identity

-- | Accepts a block identifier using the double square bracket syntax: an
-- identifier surrounded by "@[[@" and "@]]@". The identifier must start with a
-- letter, or "@_@", or "@:@". It can contain letters, digits, and some other
-- special characters defined in https://www.w3.org/TR/REC-xml/#NT-Name
--
-- Note that that the aforementioned requirements only apply to double square
-- bracket syntax: other ways of defining identifiers accept the full range of
-- Unicode characters. For more information see
-- https://asciidoctor.org/docs/user-manual/#custom-ids.
--
-- Accepts null identifiers (the empty string).
--
-- It does not accept spaces between the square brackets.
blockId :: LineParser Text
blockId =
  string "[["
    *> PC.option
      T.empty
      ( satisfy isBlockIdStartChar <> many (Parsec.satisfy isBlockIdChar)
      )
      <* string "]]"
  where
    -- TODO. Assess if package charset can be used to speed-up the following
    -- checks.
    isBlockIdStartChar c = c `elem` ['_', ':'] || isLetter c
    isBlockIdChar c =
      isBlockIdStartChar c
        || isDigit c
        || c == '-'
        || c == '.'
        -- #xB7
        || c == '·'
        -- #x203F
        || c == '‿'
        -- #x2040
        || c == '⁀'
        -- #x0300-#x036F (Combining Diacritical Marks)
        || (ord c >= 768 && ord c <= 879)

-- | Accepts an square-bracket-enclosed string with almost no restrictions on
-- the characters in between. Only the very first character needs to be alphanum
-- or one of the following list: @[\',', \'.', \'#', \'%', \'_', \'º', \'ª',
-- \'\'', \'"']@. This list has been discovered empirically (testing).
--
-- The input can be empty.
--
-- The input can contain newline characters, even if this function is usually
-- called over one line strings (Asciidoctor doesn't support multi-line block
-- attribute lists).
blockAttributeList :: LineParser Text
blockAttributeList = do
  t <- string "[" *> anyRemainder
  let (t', remainder) = T.breakOnEnd "]" t
  case (T.all isSpace remainder, T.uncons t', T.unsnoc t') of
    -- No chars between square brackets: empty attribute list
    (True, Nothing, Just (_, ']')) -> pure ""
    -- There are some chars between square brackets: accept them if starting
    -- char belongs to a restricted group of characters.
    (True, Just (s, _), Just (t'', ']')) | isStartChar s -> pure t''
    -- Fail otherwise (no square bracket at the end, or no correct first char).
    _ -> PC.empty
  where
    isStartChar c =
      isAlphaNum c || c `elem` [',', '.', '#', '%', '_', 'º', 'ª', '\'', '"']

-- | @runOfN n cs@ creates a list of parsers, one for every character @c@ member
-- of @cs@. Each of these parsers accepts any run of @n@ or more consecutive
-- appearances of @c@.
--
-- Example: @runOfN 4 [\'+', \'=']@ accepts runs of four or more symbols @"+"@, or
-- four or more symbols @"="@.
runOfN :: Int -> [SpecialChar a] -> [LineParser (Marker a)]
runOfN n = fmap $ \c ->
  (\t -> c :* (n + T.length t))
    <$ PC.count n (Parsec.char $ fromSpecialChar c)
      <*> many (Parsec.char $ fromSpecialChar c)

-- | Returns (parses successfully) the remaining text of the line, whatever its
-- content.
anyRemainder :: LineParser Text
-- anyRemainder = Parsec.getInput <* Parsec.setInput ""
anyRemainder = many Parsec.anyChar

many :: MonadPlus m => m Char -> m Text
many p = T.pack <$> PC.many p

manyText :: MonadPlus m => m Text -> m Text
manyText p = T.concat <$> PC.many p

some :: MonadPlus m => m Char -> m Text
some p = T.pack . NE.toList <$> PC.some p

count :: Int -> SpecialChar a -> LineParser (Marker a)
count m c = c :* m <$ PC.count m (Parsec.char (fromSpecialChar c))

string :: String -> LineParser Text
string s = T.pack <$> Parsec.string s

char :: Char -> LineParser Text
char c = T.singleton <$> Parsec.char c

satisfy :: (Char -> Bool) -> LineParser Text
satisfy f = T.singleton <$> Parsec.satisfy f
