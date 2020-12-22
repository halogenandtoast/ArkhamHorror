module Arkham.Types.Window where

import ClassyPrelude

import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Data.Aeson

data Window
  = AfterAssignedHorror Who
  | AfterDiscoveringClues Who Where -- name conflict resolution
  | AfterEndTurn Who
  | AfterEnemyEngageInvestigator Who EnemyId
  | AfterEnemyEvaded Who EnemyId
  | AfterFailSkillTest Who Int
  | AfterFailSkillTestAtOrLess Who Int
  | AfterPassSkillTest Source Who Int
  | AfterPlayCard Who [Trait]
  | AfterPutLocationIntoPlay Who
  | AfterRevealLocation Who
  | AfterTurnBegins Who
  | FastPlayerWindow
  | AnyPhaseBegins
  | DuringTurn Who
  | NonFast
  | WhenAllDrawEncounterCard
  | WhenAmongSearchedCards Who
  | WhenDefeated Source
  | WhenDiscoverClues Who Where
  | WhenDrawToken Who Token
  | WhenDrawTreachery Who
  | WhenDrawNonPerilTreachery Who TreacheryId
  | WhenEnemyAttacks Who
  | WhenEnemySpawns Where [Trait]
  | WhenEnemyDefeated Who
  | WhenEnemyEvaded Who
  | WhenPlayCard Who CardId
  | WhenRevealTokenWithNegativeModifier Who TokenId
  | WhenRevealToken Who Token
  | WhenSkillTest SkillType
  | WhenWouldFailSkillTest Who
  | WhenWouldRevealChaosToken Source Who
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Where = YourLocation | ConnectedLocation | LocationInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation | InvestigatorAtAConnectedLocation | InvestigatorInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
