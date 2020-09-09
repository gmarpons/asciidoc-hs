module Main
  ( main,
  )
where

import qualified Data.Aeson.Text as Aeson
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.AsciiDoc.Inlines
import Text.AsciiDoc.Pandoc
import Text.AsciiDoc.SourceRange
import qualified Text.Parsec as Parsec (runParser)

main :: IO ()
main = do
  result <- Parsec.runParser (pInlines) initialState "" <$> T.getContents
  case result of
    Left err -> error $ "Parsing error: " <> show err
    Right inline ->
      LT.putStrLn $ Aeson.encodeToLazyText $ convertDocument [addSourceRanges inline]
