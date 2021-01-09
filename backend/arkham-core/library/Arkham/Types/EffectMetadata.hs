module Arkham.Types.EffectMetadata
  ( EffectMetadata(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Modifier
import Arkham.Types.Target

data EffectMetadata a = EffectInt Int | EffectMessages [a] | EffectModifiers [Modifier] | EffectCardCode CardCode | EffectMetaTarget Target | EffectAbility Ability
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
