{-# LANGUAGE TypeApplications #-}
module Value
  ( Value(..)
  , Record(..)
  , ValueType(..)
  , TypedValue(..), valueWithType, valueOfType
  , SomeValueType(..), valueType
  , FileType(..)
  )
where

import Prelude hiding (get, put)

import Data.Vector.Binary ()
import qualified Data.HashMap.Strict as Map
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
  Boolean :: Bool -> Value
  Array :: List Value -> Value
  Record :: Record -> Value
  -- | The output of a template or of the body of a markdown document.
  Output :: StorableOutput -> Value
  deriving stock ( Generic, Show, Eq )
  deriving anyclass ( NFData, Hashable, Binary )

-- | A 'Record' is a mapping from (textual) keys to values.
data Record
  -- | A record defined explicitly in a template, or loaded from a file.
  = Concrete (HashMap Text Value)
  -- | A reference to a 'Record'-like value that is found in a file somewhere –
  -- like a YAML file. We just have the path to the file here, and retrieve the
  -- value for a given key on demand. The 'FileType' tells us how to interpret
  -- the file -- e.g. as YAML, or something else.
  | External FileType FilePath
  deriving stock ( Show, Generic, Eq )
  deriving anyclass ( NFData, Hashable, Binary )

-- | Represents the type of a 'Value' in the template language. Currently only
-- used for error reporting.
data ValueType a where
  NumberT :: ValueType Scientific
  TextT :: ValueType Text
  BooleanT :: ValueType Bool
  ArrayT :: ValueType (List Value)
  RecordT :: ValueType Record
  OutputT :: ValueType StorableOutput

instance Eq (ValueType a) where
  _ == _ = True

data TypedValue where
  Typed :: ValueType t -> t -> TypedValue

data SomeValueType where
  SomeType :: ValueType t -> SomeValueType

instance Eq SomeValueType where
  SomeType NumberT == SomeType NumberT = True
  SomeType TextT == SomeType TextT = True
  SomeType BooleanT == SomeType BooleanT = True
  SomeType ArrayT == SomeType ArrayT = True
  SomeType RecordT == SomeType RecordT = True
  SomeType OutputT == SomeType OutputT = True
  _ == _ = False

valueType :: Value -> SomeValueType
valueType = \case
  Number _ -> SomeType NumberT
  String _ -> SomeType TextT
  Boolean _ -> SomeType BooleanT
  Array _ -> SomeType ArrayT
  Record _ -> SomeType RecordT
  Output _ -> SomeType OutputT

valueWithType :: Value -> TypedValue
valueWithType = \case
  Number n -> Typed NumberT n
  String s -> Typed TextT s
  Boolean b -> Typed BooleanT b
  Array a -> Typed ArrayT a
  Record r -> Typed RecordT r
  Output o -> Typed OutputT o

valueOfType :: ValueType t -> Value -> Maybe t
valueOfType NumberT (Number n) = Just n
valueOfType TextT (String s) = Just s
valueOfType BooleanT (Boolean b) = Just b
valueOfType ArrayT (Array l) = Just l
valueOfType RecordT (Record r) = Just r
valueOfType OutputT (Output o) = Just o
valueOfType _ _ = Nothing

instance Yaml.FromJSON Value where
  parseJSON = parseYamlValue

parseYamlValue :: Yaml.Value -> Yaml.Parser Value
parseYamlValue val =
  case val of
    Yaml.Object obj ->
      Record . Concrete <$> traverse parseYamlValue obj
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

-- | A type of file that may be interpreted as a key-value mapping, i.e. a
-- 'Record'.
data FileType
  -- | The YAML file is assumed to have an associative array at the top level
  -- with string keys.
  = YamlFile
  -- | Any YAML front matter is treated as with 'YamlFile', and the document
  -- body is available under the "body" key.
  | MarkdownFile
  -- | The output of executing the template is available under the "body" key.
  -- The argument to this constructor represents the parameters given to the
  -- template. In the abstract syntax tree ('Syntax'), this is an 'ExprH',
  -- and in a 'Value', this is a @'HashMap' 'Text' 'Value'@. The template will
  -- be parsed with the provided 'Delimiters'.
  | TemplateFile Delimiters (HashMap Text Value)
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable, NFData, Typeable, Binary )

-- | Orphan instance for use in 'Record', 'FileType', etc.
instance (Eq k, Hashable k, Binary k, Binary v) => Binary (Map.HashMap k v) where
  get = fmap Map.fromList get
  put = put . Map.toList
