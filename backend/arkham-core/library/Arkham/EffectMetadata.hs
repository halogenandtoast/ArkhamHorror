module Arkham.EffectMetadata (
  EffectMetadata (..),
) where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Card.CardCode
import Arkham.Modifier
import Arkham.Target

data EffectMetadata window a
  = EffectInt Int
  | EffectMessages [a]
  | EffectModifiers [Modifier]
  | EffectCardCode CardCode
  | EffectMetaTarget Target
  | EffectAbility (Ability, [window])
  | FailedByEffectModifiers [Modifier]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
