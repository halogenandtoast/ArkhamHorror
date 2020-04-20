{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Json
  ( module X
  , module Json
  )
where

import Control.Applicative ((<|>))
import Data.Aeson as X
import Data.Aeson.Casing
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Data.Kind
import Data.Proxy
import Data.Text (Text, pack, unpack)
import GHC.Generics
import GHC.Stack
import GHC.TypeLits
import Prelude

class TypeName f where
  typeName :: f p -> String

class InnerJSON f where
  innerJSON :: f p -> Value

class ConName f where
  conName' :: f p -> String

class TaggedParse f where
  taggedParse :: Value -> Parser (f p)

class UntaggedParse f where
  untaggedParse :: HasCallStack => Value -> Parser (f p)

class TaggedParseWithKey f where
  taggedParseWithKey :: Value -> Text -> Parser (f p)

class TaggedParseWithKeyPrefix f where
  taggedParseWithKeyPrefix :: Text -> Value -> Parser (f p)

newtype TaggedJson tag a = TaggedJson { unTagged :: a }

instance (KnownSymbol tag, Generic a, TypeName (Rep a), InnerJSON (Rep a), ConName (Rep a)) => ToJSON (TaggedJson tag a) where
  toJSON a = case innerJSON r of
    Object o -> Object
      (HashMap.insert "tag" (toJSON tag) (HashMap.insert "type" (toJSON name) o)
      )
    Null -> object ["tag" .= toJSON tag, "type" .= toJSON name]
    Array ary -> object
      ["values" .= Array ary, "tag" .= toJSON tag, "type" .= toJSON name]
    Number n ->
      object ["value" .= Number n, "tag" .= toJSON tag, "type" .= toJSON name]
    String s ->
      object ["value" .= String s, "tag" .= toJSON tag, "type" .= toJSON name]
    Bool b ->
      object ["value" .= Bool b, "tag" .= toJSON tag, "type" .= toJSON name]
   where
    r = from $ unTagged a
    name = camelCase $ drop (length cname) (conName' r)
    cname = typeName r
    tag = symbolVal (Proxy @tag)

instance (Datatype c) => TypeName (M1 D c f) where
  typeName = datatypeName

instance (Generic a, TaggedParse (Rep a)) => FromJSON (TaggedJson tag a) where
  parseJSON = (TaggedJson <$>) . fmap to . taggedParse

instance (Datatype c, TaggedParseWithKeyPrefix f) => TaggedParse (M1 D c f) where
  taggedParse = (M1 <$>) . taggedParseWithKeyPrefix tname
    where tname = pack $ typeName (undefined :: M1 D c f _p)

instance (InnerJSON f) => InnerJSON (M1 t c f) where
  innerJSON = innerJSON . unM1

instance (InnerJSON c1, InnerJSON c2) => InnerJSON (c1 :+: c2) where
  innerJSON (L1 l) = innerJSON l
  innerJSON (R1 r) = innerJSON r

instance InnerJSON U1 where
  innerJSON _ = Null

instance (TaggedParseWithKeyPrefix f) => TaggedParseWithKeyPrefix (M1 C c f) where
  taggedParseWithKeyPrefix prefix = (M1 <$>) . taggedParseWithKeyPrefix prefix

instance (TaggedParseWithKeyPrefix f) => TaggedParseWithKeyPrefix (M1 S c f) where
  taggedParseWithKeyPrefix prefix = (M1 <$>) . taggedParseWithKeyPrefix prefix

instance (FromJSON a) => TaggedParseWithKeyPrefix (K1 R a) where
  taggedParseWithKeyPrefix _ = (K1 <$>) . parseJSON

instance (TaggedParseWithKey c1, TaggedParseWithKey c2) => TaggedParseWithKeyPrefix (c1 :+: c2) where
  taggedParseWithKeyPrefix prefix obj@(Object v) = do
    key <- (prefix <>) . pack . pascalCase . unpack <$> v .: "type"
    (L1 <$> taggedParseWithKey obj key) <|> (R1 <$> taggedParseWithKey obj key)
  taggedParseWithKeyPrefix _ _ = error "impossible"

instance (TaggedParseWithKey c1, TaggedParseWithKey c2) => TaggedParseWithKey (c1 :+: c2) where
  taggedParseWithKey obj key =
    (L1 <$> taggedParseWithKey obj key) <|> (R1 <$> taggedParseWithKey obj key)

instance (Constructor c, UntaggedParse f) => TaggedParseWithKey (M1 i c f) where
  taggedParseWithKey obj key
    | key == name = M1 <$> untaggedParse obj
    | otherwise = fail "Could not parse"
    where name = pack $ conName (undefined :: M1 _i c _f _p)

instance (UntaggedParse f) => UntaggedParse (M1 S c f) where
  untaggedParse = fmap M1 . untaggedParse

instance {-# OVERLAPPING #-} UntaggedParse (K1 R Int) where
  untaggedParse (Object v) = K1 <$> (v .: "value")
  untaggedParse _ = fail "not the correct type"

instance (FromJSON a) => UntaggedParse (K1 R a) where
  untaggedParse = fmap K1 . parseJSON

instance UntaggedParse U1 where
  untaggedParse _ = pure U1

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
addFieldLabelModifier f options =
  options { fieldLabelModifier = f . fieldLabelModifier options }

addConstructorTagModifier :: (String -> String) -> Options -> Options
addConstructorTagModifier f options =
  options { constructorTagModifier = f . constructorTagModifier options }

instance (KnownSymbol symbol) => ModifyOptions (Drop symbol) where
  modifyOptions = addFieldLabelModifier (camelCase . dropPrefix)
    . addConstructorTagModifier (camelCase . dropPrefix)
    where dropPrefix = drop (length (symbolVal (Proxy @symbol)))

instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a), ModifyOptions tag) => ToJSON (Codec tag a) where
  toJSON = genericToJSON (modifyOptions @tag defaultOptions) . unCodec
  toEncoding = genericToEncoding (modifyOptions @tag defaultOptions) . unCodec

instance (Generic a, GFromJSON Zero (Rep a), ModifyOptions tag) => FromJSON (Codec tag a) where
  parseJSON =
    (Codec <$>) . genericParseJSON (modifyOptions @tag defaultOptions)
