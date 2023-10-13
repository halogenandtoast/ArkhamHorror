module Arkham.Ability.Used where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Id
import Arkham.Trait
import Arkham.Window

data UsedAbility = UsedAbility
  { usedAbility :: Ability
  , usedAbilityInitiator :: InvestigatorId
  , usedAbilityWindows :: [Window]
  , usedTimes :: Int
  , usedDepth :: Int
  , usedAbilityTraits :: Set Trait
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
