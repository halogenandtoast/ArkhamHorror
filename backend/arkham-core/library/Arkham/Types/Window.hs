module Arkham.Types.Window where

import Arkham.Types.EnemyId
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

data Window
  = AfterAssignedHorror Who
  | AfterEnemyEngageInvestigator Who EnemyId
  | AfterEnemyEvaded Who EnemyId
  | AfterFailSkillTest Who Int
  | AfterFailSkillTestAtOrLess Who Int
  | AfterPassSkillTest Who Int
  | AfterPlayCard Who [Trait]
  | AfterTurnBegins Who
  | FastPlayerWindow
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
  | WhenRevealTokenWithNegativeModifier Who
  | WhenRevealToken Who Token
  | WhenSkillTest SkillType
  | WhenWouldFailSkillTest Who
  | WhenWouldRevealChaosToken Source Who
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)


data Where = YourLocation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation | InvestigatorAtAConnectedLocation | InvestigatorInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
