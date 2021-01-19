{-# LANGUAGE FlexibleContexts #-}

module Text.AsciiDoc.Debug.ParseTest
  ( OutputType (..),
    parseFile,
    module Text.AsciiDoc.Blocks,
  )
where

import qualified Control.Exception as E
-- The following import is correct, but haskell-language-server sometimes complains.
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.AsciiDoc.Blocks
import Text.Parsec (ParsecT, Stream)
-- The following import is correct, but haskell-language-server sometimes complains.
import Text.Parsec.Free.Log (LogType, renderLog)
import Text.Parsec.Prim (runPT, runPTLog)
import qualified Text.Pretty.Simple as Pretty

data OutputType = Result | Log

-- | Usage example:
--
-- > cabal repl asciidoc-hs:library:debug-with-parsec-free <<< 'parseFile pDocument Log "input.adoc"' > output.txt
parseFile ::
  Show a =>
  ParsecT [Text] State (ReaderT LogType IO) a ->
  OutputType ->
  FilePath ->
  IO ()
parseFile parser outputType file = do
  tokens <- readTokens file
  parseTestLogWithState False parser outputType mempty tokens

readTokens :: FilePath -> IO [Text]
readTokens file = do
  t <- T.readFile file
  pure $ T.lines t

parseTestLogWithState ::
  (Stream s (ReaderT LogType IO) t, Show a, Show t) =>
  -- | If True, display every parse, not just the interesting ones
  Bool ->
  ParsecT s u (ReaderT LogType IO) a ->
  OutputType ->
  u ->
  s ->
  IO ()
parseTestLogWithState b p outputType state input = do
  lg <- newIORef []
  eres <- E.try $ runReaderT (parseTestLogWithState' p outputType state input) lg
  putStrLn $ case eres of
    Left err -> "EXCEPTION => " ++ show (err :: E.SomeException)
    Right a -> "Result => " ++ show a
  theLog <- readIORef lg
  putStrLn $ renderLog b theLog

parseTestLogWithState' ::
  (MonadIO m, MonadReader LogType m, Stream s m t, Show a, Show t) =>
  ParsecT s u m a ->
  OutputType ->
  u ->
  s ->
  m ()
parseTestLogWithState' p outputType state input = do
  eres <- case outputType of
    Result -> runPT p state "" input
    Log -> runPTLog p state "" input
  liftIO $ case eres of
    Left err -> do
      putStr "parse error at "
      print err
    Right x -> Pretty.pPrint x
