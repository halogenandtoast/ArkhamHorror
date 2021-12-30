module Arkham.DamageEffect where

import Arkham.Prelude

data DamageEffect = AttackDamageEffect | NonAttackDamageEffect
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
