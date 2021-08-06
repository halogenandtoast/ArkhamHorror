module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Id
import Arkham.Types.Phase
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token

data Window
  = AfterDiscoveringClues InvestigatorId LocationId -- name conflict resolution
  | AfterDrawCard InvestigatorId Card
  | WhenDrawCard InvestigatorId Card
  | AfterDrawingStartingHand InvestigatorId
  | AfterCommitedCard InvestigatorId Card
  | AfterEndTurn InvestigatorId
  | AfterEnemyDefeated InvestigatorId EnemyId
  | AfterEnemyEngageInvestigator InvestigatorId EnemyId
  | AfterEnemyEvaded InvestigatorId EnemyId
  | AfterFailAttackEnemy InvestigatorId EnemyId
  | AfterFailInvestigationSkillTest InvestigatorId Int
  | AfterFailSkillTest InvestigatorId Int
  | AfterFailSkillTestAtOrLess InvestigatorId Int
  | AfterLeaving InvestigatorId LocationId
  | AfterMoveFromHunter EnemyId
  | AfterEntering InvestigatorId LocationId
  | AfterPassSkillTest (Maybe Action) Source InvestigatorId Int
  | AfterPlayCard InvestigatorId Card
  | AfterPutLocationIntoPlay InvestigatorId
  | AfterRevealLocation InvestigatorId
  | AfterSuccessfulAttackEnemy InvestigatorId EnemyId
  | AfterSuccessfulInvestigation InvestigatorId LocationId
  | AfterTurnBegins InvestigatorId
  | AnyPhaseBegins
  | PhaseBegins Phase
  | PhaseEnds Phase
  | AtEndOfRound
  | DuringTurn InvestigatorId
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
  | WhenDiscoverClues InvestigatorId LocationId Int
  | WhenWouldDrawEncounterCard InvestigatorId
  | WhenDrawToken InvestigatorId Token
  | WhenEnemyAttacks InvestigatorId EnemyId
  | WhenEnemyDefeated InvestigatorId EnemyId
  | WhenEnemyEvaded InvestigatorId EnemyId
  | WhenEnemySpawns EnemyId LocationId
  | WhenEnterPlay Target
  | WhenLocationLeavesPlay LocationId
  | WhenPlayCard InvestigatorId CardId
  | WhenRevealToken InvestigatorId Token
  | AfterRevealToken InvestigatorId Token
  | WhenRevealTokenWithNegativeModifier InvestigatorId Token
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
  | WhenWouldTakeDamageOrHorror Source Target Int Int
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)
