{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text ()
import qualified Text.Asciidoc.Inlines as Inlines

main :: IO ()
main = do
  Inlines.parseTestInline "*Parse* me!"
