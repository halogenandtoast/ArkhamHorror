module Arkham.Types.FastWindow where

import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

data Where = YourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data FastWindow
  = WhenDrawTreachery Who Bool -- < is weakness
  | WhenEnemyAttacks Who
  | WhenEnemyDefeated Who
  | WhenDrawToken Who
  | DuringTurn Who
  | WhenDiscoverClues Who Where
  | WhenSkillTest SkillType
  | AfterFailSkillTest Who
  | AfterPassSkillTest Who
  | AfterAssignedHorror Who
  | AfterTurnBegins Who
  | AfterPlayCard Who [Trait]
  | Any
  | NonFast
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

