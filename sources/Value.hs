{-|
Description : The definition of values in the template expression language.
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Value
  ( Value(..)
  , ValueType(..)
  , TypedValue(..), valueWithType, valueOfType
  , SomeValueType(..), valueType
  , FileType(..), DocumentInfo(..)
  , displayValue, displayType
  )
where

import Prelude hiding (get, put)

import Data.Vector.Binary ()
import qualified Data.HashMap.Strict as Map
import Data.Scientific
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Development.Shake.Classes
import qualified Data.Yaml as Yaml

import KnownFunction
import List (List)
import Syntax
import Syntax.Display

{-| A 'Value' is a thing that may be the value of a name in a template.
Effectively, templates are dynamically typed. Similar to Aeson's 'Yaml.Value'
type.-}
data Value where
  Number :: Scientific -> Value
  String :: Text -> Value
  Boolean :: Bool -> Value
  Array :: List Value -> Value
  Record :: HashMap Text Value -> Value
  -- | The result of a call to @load-markdown@, @load-template@, etc. Documents
  -- from other files are loaded lazily – we get each field, or the document body,
  -- on demand.
  LoadedDoc :: DocumentInfo -> Value
  deriving stock ( Generic, Show, Eq )
  deriving anyclass ( NFData, Hashable, Binary )

displayValue :: Value -> Doc any
displayValue = \case
  Number n -> unsafeViaShow n
  String s -> viaShow s
  Boolean b -> pretty b
  Array a -> displayList displayValue a
  Record r ->
    let pairToBinding (n,v) = FieldAssignment (Name n) v
        binds = map pairToBinding $ Map.toList r
    in displayRecord displayValue binds
  LoadedDoc (DocInfo ft fp) ->
    displayExprF displayValue $
      case ft of
        YamlFile ->
          -- TODO FilePath Value
          FunctionCallE (Name loadYamlFunctionName) [String (Text.pack fp)]
        MarkdownFile ->
          FunctionCallE (Name loadMarkdownFunctionName) [String (Text.pack fp)]
        TemplateFile _delims binds ->
          FunctionCallE (Name loadTemplateFunctionName) [String (Text.pack fp), Record binds]

-- | A document is represented as a reference to a 'Record'-like value that is
-- found in a file somewhere – like a YAML file. The document may also have a
-- /body/ (in the case of markdown and templates). We just have the path to the
-- file here, and retrieve the value for a given key (or the document body) on
-- demand. The 'FileType' tells us how to interpret the file – e.g. as YAML, or
-- something else.
data DocumentInfo = DocInfo
  { docFileType :: FileType
  , docFilePath :: FilePath }
  deriving stock ( Show, Generic, Eq )
  deriving anyclass ( NFData, Hashable, Binary )

-- | Represents the type of a 'Value' in the template language. Used for
-- error reporting and for checking the types of function arguments.
data ValueType a where
  NumberT :: ValueType Scientific
  TextT :: ValueType Text
  BooleanT :: ValueType Bool
  ArrayT :: ValueType (List Value)
  RecordT :: ValueType (HashMap Text Value)
  DocumentT :: ValueType DocumentInfo

displayType :: SomeValueType -> Doc any
displayType (SomeType t) = pretty (typeName t)

typeName :: ValueType a -> Text
typeName = \case
  NumberT -> "number"
  TextT -> "text"
  BooleanT -> "boolean"
  ArrayT -> "array"
  RecordT -> "record"
  DocumentT -> "document"

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
  SomeType DocumentT == SomeType DocumentT = True
  _ == _ = False

instance Show SomeValueType where
  show (SomeType t) = Text.unpack (typeName t)

valueType :: Value -> SomeValueType
valueType = \case
  Number _ -> SomeType NumberT
  String _ -> SomeType TextT
  Boolean _ -> SomeType BooleanT
  Array _ -> SomeType ArrayT
  Record _ -> SomeType RecordT
  LoadedDoc _ -> SomeType DocumentT

valueWithType :: Value -> TypedValue
valueWithType = \case
  Number n -> Typed NumberT n
  String s -> Typed TextT s
  Boolean b -> Typed BooleanT b
  Array a -> Typed ArrayT a
  Record r -> Typed RecordT r
  LoadedDoc d -> Typed DocumentT d

valueOfType :: ValueType t -> Value -> Maybe t
valueOfType NumberT (Number n) = Just n
valueOfType TextT (String s) = Just s
valueOfType BooleanT (Boolean b) = Just b
valueOfType ArrayT (Array l) = Just l
valueOfType RecordT (Record r) = Just r
valueOfType DocumentT (LoadedDoc d) = Just d
valueOfType _ _ = Nothing

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

-- | A type of file that may be interpreted as a key-value mapping, i.e. a
-- 'Record', and which may also have a document body.
data FileType
  -- | The YAML file is assumed to have an associative array at the top level
  -- with string keys.
  = YamlFile
  -- | Any YAML front matter is treated as with 'YamlFile', and the document
  -- body is available with @include-body@ ('IncludeBodyS').
  | MarkdownFile
  -- | The output of executing the template is the document body, accessible via
  -- the @include-body@ statement ('IncludeBodyS'). The second argument to this
  -- constructor represents the parameters given to the template. The template
  -- will be parsed with the 'Delimiters' provided as the first argument.
  | TemplateFile Delimiters (HashMap Text Value)
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable, NFData, Typeable, Binary )

-- | Orphan instance for use in 'Record', 'FileType', etc.
instance (Eq k, Hashable k, Binary k, Binary v) => Binary (Map.HashMap k v) where
  get = fmap Map.fromList get
  put = put . Map.toList
