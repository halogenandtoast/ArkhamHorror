{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Json where

import Prelude ((.), String, (<$>), length, drop)
import Data.Aeson
import Data.Aeson.Casing
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

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
