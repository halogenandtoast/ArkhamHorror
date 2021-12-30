module Arkham.EffectMetadata (
  EffectMetadata (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Modifier
import Arkham.Target
import {-# SOURCE #-} Arkham.Window (Window)

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
