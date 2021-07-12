module Arkham.Types.PlayRestriction where

import Arkham.Prelude

data PlayRestriction
  = AnotherInvestigatorInSameLocation
  | ScenarioCardHasResignAbility
  | ClueOnLocation
  | EnemyAtYourLocation
  | OwnCardWithDoom
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
