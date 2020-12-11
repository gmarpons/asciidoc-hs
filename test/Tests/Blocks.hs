module Tests.Blocks
  ( parseTest,
  )
where

import Data.Functor.Identity
import Data.Text (Text)
import Text.AsciiDoc.Blocks (Parser)
import qualified Text.Parsec as Parsec

parseTest :: Parser Identity a -> [Text] -> Either Parsec.ParseError a
parseTest parser tokens =
  runIdentity $ Parsec.runParserT parser mempty "" tokens
