module Arkham.EffectMetadata where

import Data.Kind (Type)

type EffectMetadata :: Type -> Type -> Type
data EffectMetadata window a
