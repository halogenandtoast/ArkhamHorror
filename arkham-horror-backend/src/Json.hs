{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Json where

import Prelude ((.), ($), String, (<$>), length, drop, error)
import Data.Aeson
import Data.Aeson.Casing
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import qualified Data.HashMap.Strict as HashMap

class InnerJSON f where
  innerJSON :: f p -> Value

class ConName f where
  conName' :: f p -> String

taggedToJSON :: (Generic a, InnerJSON (Rep a), ConName (Rep a)) => a -> Value
taggedToJSON a = case innerJSON r of
  Object o -> Object (HashMap.insert "tag" (toJSON $ conName' r) o)
  _        -> error "impossible"
  where r = from a

newtype TaggedJson a = TaggedJson { unTagged :: a }

instance (Generic a, InnerJSON (Rep a), ConName (Rep a)) => ToJSON (TaggedJson a) where
  toJSON = taggedToJSON . unTagged

instance (InnerJSON f) => InnerJSON (M1 t c f) where
  innerJSON = innerJSON . unM1

instance (InnerJSON c1, InnerJSON c2) => InnerJSON (c1 :+: c2) where
  innerJSON (L1 l) = innerJSON l
  innerJSON (R1 r) = innerJSON r

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
