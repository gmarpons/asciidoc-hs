module Main where

import Data.Text ()
import qualified Text.AsciiDoc.Inlines as Inlines

main :: IO ()
main = do
  Inlines.parseTestInlines "Test" "*Parse* me!"
