module Arkham.Effect.Types where

import Arkham.Prelude

data Effect
data EffectAttrs

instance Show EffectAttrs
instance Eq EffectAttrs
instance Ord EffectAttrs
instance Data EffectAttrs
instance ToJSON EffectAttrs
instance FromJSON EffectAttrs

data EffectBuilder

instance Show EffectBuilder
instance Eq EffectBuilder
instance Ord EffectBuilder
instance Data EffectBuilder
instance ToJSON EffectBuilder
instance FromJSON EffectBuilder
