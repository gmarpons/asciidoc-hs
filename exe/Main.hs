module Main where

import qualified Data.Aeson.Text as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import Text.AsciiDoc.Inlines
import Text.AsciiDoc.Pandoc
import qualified Text.Parsec as Parsec (parse)

main :: IO ()
main = do
  result <- Parsec.parse pInlines "" . T.pack <$> getLine
  case result of
    Left err -> error $ "Parsing error: " <> show err
    Right inlines ->
      T.putStrLn $ Aeson.encodeToLazyText $ convertDocument (NE.toList inlines)
