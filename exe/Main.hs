module Main
  ( main,
  )
where

import qualified Data.Aeson.Text as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import Text.AsciiDoc.Inlines
import Text.AsciiDoc.Pandoc
import qualified Text.Parsec as Parsec (runParser)

main :: IO ()
main = do
  result <- Parsec.runParser (pInlines) initialState "" . T.pack <$> getContents
  case result of
    Left err -> error $ "Parsing error: " <> show err
    Right inline ->
      T.putStrLn $ Aeson.encodeToLazyText $ convertDocument [addSourcePositions inline]
