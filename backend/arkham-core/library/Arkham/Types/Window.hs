module Arkham.Types.Window where

import Arkham.Types.SkillType
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

data Where = YourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Window
  = AfterAssignedHorror Who
  | AfterFailSkillTest Who Int
  | AfterFailSkillTestAtOrLess Who Int
  | AfterPassSkillTest Who Int
  | AfterPlayCard Who [Trait]
  | AfterTurnBegins Who
  | Any
  | AnyPhaseBegins
  | DuringTurn Who
  | NonFast
  | WhenDiscoverClues Who Where
  | WhenDrawToken Who Token
  | WhenDrawTreachery Who Bool -- < is weakness
  | WhenEnemyAttacks Who
  | WhenEnemySpawns Where [Trait]
  | WhenEnemyDefeated Who
  | WhenEnemyEvaded Who
  | WhenSkillTest SkillType
  | WhenWouldFailSkillTest Who
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

