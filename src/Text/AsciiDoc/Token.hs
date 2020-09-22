-- |
-- Module      :  Text.AsciiDoc.Token
-- Copyright   :  © 2020–present Guillem Marpons
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Guillem Marpons <gmarpons@mailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Token type to be used as input of the 'Text.Asciidoc.Blocks.Block' parser.
module Text.AsciiDoc.Token
  ( -- * Token datatype
    Token (..),

    -- * Tokenize function
    tokenize,
  )
where

import Data.Text (Text)
import Text.AsciiDoc.Inlines
import Text.AsciiDoc.Preprocessor

-- | Typical instantiation: @Token (Wrapped Text Text)@.
data Token a
  = UnparsedLine a
  | -- TODO. FilePath and IncludeOptions could need to be Wrapped.
    Include FilePath IncludeOptions
  | EndOfInclude
  | -- | @AttributeEntry key value@. The value is in full expanded form.
    --
    -- The value can contain @Newline@s and space.
    AttributeEntry AttributeId Inline
  deriving (Eq, Show)

-- | Not pure because of include expansion.
tokenize :: Text -> IO [Token Text]
tokenize t = fmap pseudoLine2Token <$> preprocess MainFile t
  where
    pseudoLine2Token :: PreprocessedLine -> Token Text
    pseudoLine2Token = \case
      ExpandedAttribute k v -> AttributeEntry k v
      EndOfIncludePseudoLine -> EndOfInclude
      Preprocessed (IncludeLine path options) -> Include path options
      Preprocessed (OtherLine t') -> UnparsedLine t'
      Init -> EndOfInclude -- error "pseudoLine2token: TODO. Init found"
      OpenAttribute _ _ -> error "pseudoLine2token: OpenAttribute found"
