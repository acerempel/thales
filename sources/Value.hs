{-# OPTIONS_GHC -Wno-orphans #-}
module Value ( Value(..) ) where

import Control.Foldl as Fold
import Data.Scientific
import qualified Data.Yaml as Yaml
import Text.MMark as MMark
import Text.MMark.Extension as MMark
import Text.URI

import List (List)
import Output (Output)

{-| A 'Value' is a thing that may be the value of a name in a template.
Effectively, templates are dynamically typed. Aside from the absence of null,
this is basically equivalent to Aeson's @Value@
type.-}
data Value where
  Number :: Scientific -> Value
  String :: Text -> Value
  Verbatim :: Text -> Value
  Boolean :: Bool -> Value
  Array :: List Value -> Value
  Record :: HashMap Text Value -> Value
  -- | A Markdown document.
  Markdown :: MMark -> Value
  -- | A reference to a 'Record'-like value that is found in a file somewhere –
  -- like a YAML file. We just have the path to the file here, and retrieve the
  -- value for a given key on demand.
  ExternalRecord :: FilePath -> Value
  deriving stock ( Generic, Typeable, Show, Eq )
  deriving anyclass ( NFData, Hashable )

-- | Orphan instance, necessary for @Eq 'Value'@.
instance Eq MMark where
  (==) =
    (==) `on` flip MMark.runScanner Fold.list

instance Hashable a => Hashable (MMark.Block a)
instance Hashable MMark.Inline
instance Hashable MMark.CellAlign
instance Hashable URI
instance Hashable (RText label)
instance Hashable Authority
instance Hashable UserInfo
instance Hashable QueryParam

-- | Orphan instance, needed for @Hashable 'Value'@.
instance Hashable MMark where
  hashWithSalt salt mmark =
    hashWithSalt salt (MMark.runScanner mmark Fold.list)

instance Yaml.FromJSON Value where
  parseJSON = yamlValueToValue

yamlValueToValue :: Yaml.Value -> Yaml.Parser Value
yamlValueToValue val =
  case val of
    Yaml.Object obj ->
      Record <$> traverse yamlValueToValue obj
    Yaml.Array arr ->
      Array <$> traverse yamlValueToValue arr
    Yaml.String str ->
      pure $ String str
    Yaml.Number num ->
      pure $ Number num
    Yaml.Bool boo ->
      pure $ Boolean boo
    Yaml.Null ->
      fail "Null not supported!"
