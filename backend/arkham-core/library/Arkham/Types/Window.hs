module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.ActId
import Arkham.Types.Action
import Arkham.Types.AgendaId
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId

data Window
  = AfterDiscoveringClues Who Where -- name conflict resolution
  | AfterDrawCard Who CardId
  | AfterEndTurn Who
  | AfterEnemyDefeated Who EnemyId
  | AfterEnemyEngageInvestigator Who EnemyId
  | AfterEnemyEvaded Who EnemyId
  | AfterFailSkillTest Who Int
  | AfterFailInvestigationSkillTest Who Int
  | AfterFailSkillTestAtOrLess Who Int
  | AfterPassSkillTest (Maybe Action) Source Who Int
  | AfterPlayCard Who [Trait]
  | AfterPutLocationIntoPlay Who
  | AfterRevealLocation Who
  | AfterSuccessfulInvestigation Who Where
  | AfterSuccessfulAttackEnemy Who EnemyId
  | AfterTurnBegins Who
  | FastPlayerWindow
  | AnyPhaseBegins
  | DuringTurn Who
  | NonFast
  | WhenActAdvance ActId
  | WhenAgendaAdvance AgendaId
  | WhenAllDrawEncounterCard
  | WhenAmongSearchedCards Who
  | WhenDealtDamage Source Target
  | WhenDealtHorror Source Target
  | WhenDefeated Source
  | WhenDiscoverClues Who Where
  | WhenDrawToken Who Token
  | WhenDrawTreachery Who
  | WhenDrawNonPerilTreachery Who TreacheryId
  | WhenEnemyAttacks Who
  | WhenEnemySpawns Where [Trait]
  | WhenEnemyDefeated Who
  | WhenEnemyEvaded Who
  | WhenEnterPlay Target
  | WhenWouldReady Target
  | WhenPlayCard Who CardId
  | WhenRevealTokenWithNegativeModifier Who TokenId
  | WhenRevealToken Who Token
  | WhenSkillTest SkillType
  | WhenSuccessfulAttackEnemy Who EnemyId
  | WhenSuccessfulInvestigation Who Where
  | WhenWouldFailSkillTest Who
  | WhenWouldRevealChaosToken Source Who
  | WhenWouldTakeDamage Source Target
  | InHandWindow InvestigatorId Window
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Where = YourLocation | ConnectedLocation | LocationInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation | InvestigatorAtAConnectedLocation | InvestigatorInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
