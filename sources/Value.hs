{-# LANGUAGE TypeApplications #-}
module Value ( Value(..), ValueType(..) ) where

import Data.Binary.Instances.UnorderedContainers ()
import Data.Binary.Instances.Vector ()
import Data.Scientific
import Development.Shake.Classes
import qualified Data.Yaml as Yaml

import List (List)
import Output (StorableOutput)
import Syntax

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
  -- | The output of a template or of the body of a markdown document.
  Output :: StorableOutput -> Value
  -- | A reference to a 'Record'-like value that is found in a file somewhere –
  -- like a YAML file. We just have the path to the file here, and retrieve the
  -- value for a given key on demand. The 'FileType' tells us how to interpret
  -- the file -- e.g. as YAML, or something else.
  ExternalRecord :: FileType (HashMap Text Value) -> FilePath -> Value
  deriving stock ( Generic, Typeable, Show, Eq )
  deriving anyclass ( NFData, Hashable, Binary )

data ValueType
  = NumberT
  | TextT
  | BooleanT
  | ArrayT
  | RecordT
  | OutputT
  deriving ( Show, Eq, Generic )

instance Yaml.FromJSON Value where
  parseJSON = parseYamlValue

parseYamlValue :: Yaml.Value -> Yaml.Parser Value
parseYamlValue val =
  case val of
    Yaml.Object obj ->
      Record <$> traverse parseYamlValue obj
    Yaml.Array arr ->
      Array <$> traverse parseYamlValue arr
    Yaml.String str ->
      pure $ String str
    Yaml.Number num ->
      pure $ Number num
    Yaml.Bool boo ->
      pure $ Boolean boo
    Yaml.Null ->
      fail "Null not supported!"
