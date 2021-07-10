module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Card.Id
import Arkham.Types.Id
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token hiding (TokenId)
import Arkham.Types.Trait

data Window
  = AfterDiscoveringClues InvestigatorId LocationId -- name conflict resolution
  | AfterDrawCard InvestigatorId CardId
  | AfterEndTurn InvestigatorId
  | AfterEnemyDefeated InvestigatorId EnemyId
  | AfterEnemyDefeatedOfType InvestigatorId Trait
  | AfterEnemyEngageInvestigator InvestigatorId EnemyId
  | AfterFailInvestigationSkillTest InvestigatorId Int
  | AfterFailSkillTest InvestigatorId Int
  | AfterFailSkillTestAtOrLess InvestigatorId Int
  | AfterLeaving InvestigatorId LocationId
  | AfterPassSkillTest (Maybe Action) Source InvestigatorId Int
  | AfterPlayCard InvestigatorId [Trait]
  | AfterPutLocationIntoPlay InvestigatorId
  | AfterRevealLocation InvestigatorId
  | AfterSuccessfulAttackEnemy InvestigatorId EnemyId
  | AfterSuccessfulInvestigation InvestigatorId LocationId
  | AfterTurnBegins InvestigatorId
  | AnyPhaseBegins
  | AtEndOfRound
  | FastPlayerWindow
  | InDiscardWindow InvestigatorId Window
  | InHandWindow InvestigatorId Window
  | NonFast
  | WhenActAdvance ActId
  | WhenAgendaAdvance AgendaId
  | WhenAllDrawEncounterCard
  | WhenAmongSearchedCards InvestigatorId
  | WhenChosenRandomLocation LocationId
  | WhenDealtDamage Source Target
  | WhenDealtHorror Source Target
  | WhenDefeated Source
  | WhenDiscoverClues InvestigatorId LocationId
  | WhenDrawNonPerilTreachery InvestigatorId TreacheryId
  | WhenDrawToken InvestigatorId Token
  | WhenDrawTreachery InvestigatorId
  | WhenEnemyAttacks InvestigatorId
  | WhenEnemyDefeated InvestigatorId
  | WhenEnemyEvaded InvestigatorId
  | WhenEnemySpawns LocationId [Trait]
  | WhenEnterPlay Target
  | WhenLocationLeavesPlay LocationId
  | WhenPlayCard InvestigatorId CardId
  | WhenRevealToken InvestigatorId Token
  | WhenRevealTokenWithNegativeModifier InvestigatorId TokenId
  | WhenSkillTest SkillType
  | WhenSuccessfulAttackEnemy InvestigatorId EnemyId
  | WhenSuccessfulInvestigation InvestigatorId LocationId
  | WhenTurnBegins InvestigatorId
  | WhenWouldFailSkillTest InvestigatorId
  | WhenWouldLeave InvestigatorId LocationId
  | WhenWouldReady Target
  | WhenWouldRevealChaosToken Source InvestigatorId
  | WhenWouldTakeDamage Source Target
  | WhenWouldTakeHorror Source Target
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
