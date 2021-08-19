module Arkham.Types.EffectMetadata
  ( EffectMetadata(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Window (Window)

data EffectMetadata a
  = EffectInt Int
  | EffectMessages [a]
  | EffectModifiers [Modifier]
  | EffectCardCode CardCode
  | EffectMetaTarget Target
  | EffectAbility (Ability, [Window])
  | FailedByEffectModifiers [Modifier]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
