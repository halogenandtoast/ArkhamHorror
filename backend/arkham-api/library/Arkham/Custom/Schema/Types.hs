-- | The shape of the schema served to the ability editor. See "Arkham.Custom.Schema".
module Arkham.Custom.Schema.Types where

import Arkham.Prelude
import Language.Haskell.TH.Syntax (Lift)

data FieldSchema = FieldSchema
  { fieldName :: Maybe Text
  -- ^ Record field name, absent for positional constructors.
  , fieldType :: Text
  -- ^ Rendered type, e.g. @Int@, @[EnemyMatcher]@, @Maybe Text@.
  }
  deriving stock (Show, Eq, Lift)

data ConSchema = ConSchema
  { conName :: Text
  , conFields :: [FieldSchema]
  }
  deriving stock (Show, Eq, Lift)

data TypeSchema = TypeSchema
  { typeName :: Text
  , typeConstructors :: [ConSchema]
  , typeIsRecord :: Bool
  {- ^ Records encode their fields inline next to the tag; positional
  constructors put theirs in @contents@.
  -}
  , typeAlias :: Maybe Text
  {- ^ A type synonym stands for another type -- @Who@ is an
  'InvestigatorMatcher', @Where@ a 'LocationMatcher'. Without this the editor
  sees a name it has no constructors for and falls back to a raw field.
  -}
  , typeIsEnum :: Bool
  {- ^ Every constructor is nullary, which aeson encodes as a bare string rather
  than a tagged object (@allNullaryToStringTag@). The editor has to match that or
  the value silently fails to decode.
  -}
  }
  deriving stock (Show, Eq, Lift)

instance ToJSON FieldSchema where
  toJSON f = object ["name" .= fieldName f, "type" .= fieldType f]

instance ToJSON ConSchema where
  toJSON c = object ["name" .= conName c, "fields" .= conFields c]

instance ToJSON TypeSchema where
  toJSON t =
    object
      [ "name" .= typeName t
      , "constructors" .= typeConstructors t
      , "record" .= typeIsRecord t
      , "enum" .= typeIsEnum t
      , "alias" .= typeAlias t
      ]
