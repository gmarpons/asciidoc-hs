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
    runOfN,
    many,
    some,
    count,
    string,
    satisfy,
    remaining,
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
import Data.Functor.Identity (Identity)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec

-- | Parser type used to check syntactic conditions on single lines of an
-- AsciiDoc document.
type LineParser = Parsec.ParsecT Text () Identity

-- | @runOfN n cs@ creates a list of parsers, one for every character @c@ member
-- of @cs@. Each of these parsers accepts any run of @n@ or more consecutive
-- appearances of @c@.
--
-- Example: @runOfN 4 ['+', '=']@ accepts runs of four or more symbols @"+"@, or
-- four or more symbols @"="@.
runOfN :: Int -> [Char] -> [LineParser Text]
runOfN n = fmap $ \c -> count n (Parsec.char c) <> many (Parsec.char c)

many :: MonadPlus f => f Char -> f Text
many p = T.pack <$> PC.many p

some :: MonadPlus f => f Char -> f Text
some p = (T.pack . NE.toList) <$> PC.some p

count :: Monad f => Int -> f Char -> f Text
count m p = T.pack <$> PC.count m p

string :: String -> LineParser Text
string s = T.pack <$> Parsec.string s

satisfy :: (Char -> Bool) -> LineParser Text
satisfy f = T.singleton <$> Parsec.satisfy f

-- | Returns (parses successfully) the remaining text of line, whatever its
-- content.
remaining :: LineParser Text
remaining = Parsec.getInput <* Parsec.setInput ""
