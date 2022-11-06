module Arkham.DamageEffect where

import Arkham.Prelude

data DamageEffect
  = AttackDamageEffect
  | NonAttackDamageEffect
  | StoryCardDamageEffect
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
