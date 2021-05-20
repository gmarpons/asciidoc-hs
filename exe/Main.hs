module Main
  ( main,
  )
where

import qualified Data.Aeson.Text as Aeson
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.AsciiDoc.Blocks
import Text.AsciiDoc.Pandoc
import qualified Text.Parsec as Parsec (runParser)

main :: IO ()
main = do
  result <-
    Parsec.runParser documentP blockParserInitialState ""
      . T.lines
      <$> T.getContents
  case result of
    Left err -> error $ "Parsing error: " <> show err
    Right doc ->
      LT.putStrLn $ Aeson.encodeToLazyText $ convertDocument $ parseInlines doc
