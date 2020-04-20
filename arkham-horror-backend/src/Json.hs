{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Json ( module X, module Json) where

import Prelude
import Control.Applicative ((<|>))
import Data.Aeson as X
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Data.Text (pack, Text)
import qualified Data.HashMap.Strict as HashMap

class TypeName f where
  typeName :: f p -> String

class InnerJSON f where
  innerJSON :: f p -> Value

class ConName f where
  conName' :: f p -> String

class TaggedParse f where
  taggedParse :: Value -> Parser (f p)

class UntaggedParse f where
  untaggedParse :: Value -> Parser (f p)

class TaggedParseWithKey f where
  taggedParseWithKey :: Value -> Text -> Parser (f p)

taggedToJSON :: (Generic a, TypeName (Rep a), InnerJSON (Rep a), ConName (Rep a)) => String -> a -> Value
taggedToJSON tag a = case innerJSON r of
  Object o -> Object (HashMap.insert "tag" (toJSON tag) (HashMap.insert "type" (toJSON name) o))
  Number n -> object ["value" .= Number n,  "tag" .= toJSON tag, "type" .= toJSON name ]
  _        -> error "impossible: not an object?"
  where r = from a
        name = camelCase $ drop (length cname) (conName' r)
        cname = typeName r

taggedParseJSON :: (Generic a, TaggedParse (Rep a)) => Value -> Parser a
taggedParseJSON = fmap to . taggedParse

newtype TaggedJson tag a = TaggedJson { unTagged :: a }

instance (KnownSymbol tag, Generic a, TypeName (Rep a), InnerJSON (Rep a), ConName (Rep a)) => ToJSON (TaggedJson tag a) where
  toJSON = taggedToJSON tag . unTagged
    where tag = symbolVal (Proxy @tag)

instance (Datatype c) => TypeName (M1 D c f) where
  typeName = datatypeName

instance (Generic a, TaggedParse (Rep a)) => FromJSON (TaggedJson tag a) where
  parseJSON = (TaggedJson <$>) . taggedParseJSON

instance (TaggedParse f) => TaggedParse (M1 i c f) where
  taggedParse = (M1 <$>) . taggedParse

instance (FromJSON a) => TaggedParse (K1 R a) where
  taggedParse = (K1 <$>) . parseJSON

instance (InnerJSON f) => InnerJSON (M1 t c f) where
  innerJSON = innerJSON . unM1

instance (InnerJSON c1, InnerJSON c2) => InnerJSON (c1 :+: c2) where
  innerJSON (L1 l) = innerJSON l
  innerJSON (R1 r) = innerJSON r

instance (TaggedParseWithKey c1, TaggedParseWithKey c2) => TaggedParse (c1 :+: c2) where
  taggedParse obj@(Object v) = do
    key <- v .: "type"
    (L1 <$> taggedParseWithKey obj key) <|>
      (R1 <$> taggedParseWithKey obj key)
  taggedParse _ = error "impossible"

instance (TaggedParseWithKey c1, TaggedParseWithKey c2) => TaggedParseWithKey (c1 :+: c2) where
  taggedParseWithKey obj key =
    (L1 <$> taggedParseWithKey obj key) <|>
      (R1 <$> taggedParseWithKey obj key)

instance (Constructor c, UntaggedParse f) => TaggedParseWithKey (M1 i c f) where
  taggedParseWithKey obj key
    | key == name = M1 <$> untaggedParse obj
    | otherwise = fail "Could not parse"
    where
      name = pack $ conName (undefined :: M1 _i c _f _p)

instance (UntaggedParse f) => UntaggedParse (M1 S c f) where
  untaggedParse = fmap M1 . untaggedParse

instance (FromJSON a) => UntaggedParse (K1 R a) where
  untaggedParse = fmap K1 . parseJSON

instance (ToJSON a) => InnerJSON (K1 R a) where
  innerJSON = toJSON . unK1

instance (ConName f) => ConName (M1 D c f) where
  conName' = conName' . unM1

instance (ConName c1, ConName c2) => ConName (c1 :+: c2) where
  conName' (L1 l) = conName' l
  conName' (R1 r) = conName' r

instance (Constructor c) => ConName (M1 C c f) where
  conName' = conName

newtype Codec (tag :: k) (value :: Type) = Codec { unCodec :: value }
data Drop symbol

class ModifyOptions tag where
  modifyOptions :: Options -> Options

addFieldLabelModifier :: (String -> String) -> Options -> Options
addFieldLabelModifier f options = options
  { fieldLabelModifier = f . fieldLabelModifier options
  }

addConstructorTagModifier :: (String -> String) -> Options -> Options
addConstructorTagModifier f options = options
    { constructorTagModifier = f . constructorTagModifier options
    }

instance (KnownSymbol symbol) => ModifyOptions (Drop symbol) where
  modifyOptions
      = addFieldLabelModifier (camelCase . dropPrefix)
      . addConstructorTagModifier (camelCase . dropPrefix)
    where
      dropPrefix = drop (length (symbolVal (Proxy @symbol)))

instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a), ModifyOptions tag) => ToJSON (Codec tag a) where
  toJSON = genericToJSON (modifyOptions @tag defaultOptions) . unCodec
  toEncoding = genericToEncoding (modifyOptions @tag defaultOptions) . unCodec

instance (Generic a, GFromJSON Zero (Rep a), ModifyOptions tag) => FromJSON (Codec tag a) where
  parseJSON = (Codec <$>) . genericParseJSON (modifyOptions @tag defaultOptions)
