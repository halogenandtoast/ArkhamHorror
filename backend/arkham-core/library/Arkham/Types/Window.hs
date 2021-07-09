module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Card.Id
import Arkham.Types.Id
import Arkham.Types.Keyword
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token hiding (TokenId)
import Arkham.Types.Trait

data Window
  = WindowAfterDiscoveringClues InvestigatorId LocationId -- name conflict resolution
  | WindowAfterDrawCard InvestigatorId CardId
  | WindowAfterEndTurn InvestigatorId
  | WindowAfterEnemyDefeated InvestigatorId EnemyId
  | WindowAfterEnemyDefeatedOfType InvestigatorId Trait
  | WindowAfterEnemyEngageInvestigator InvestigatorId EnemyId
  | WindowAfterFailInvestigationSkillTest InvestigatorId Int
  | WindowAfterFailSkillTest InvestigatorId Int
  | WindowAfterFailSkillTestAtOrLess InvestigatorId Int
  | WindowAfterLeaving InvestigatorId LocationId
  | WindowAfterPassSkillTest (Maybe Action) Source InvestigatorId Int
  | WindowAfterPlayCard InvestigatorId [Trait]
  | WindowAfterPutLocationIntoPlay InvestigatorId
  | WindowAfterRevealLocation InvestigatorId
  | WindowAfterSuccessfulAttackEnemy InvestigatorId EnemyId
  | WindowAfterSuccessfulInvestigation InvestigatorId LocationId
  | WindowAfterTurnBegins InvestigatorId
  | WindowAnyPhaseBegins
  | WindowAtEndOfRound
  | WindowFastPlayerWindow
  | WindowInDiscardWindow InvestigatorId Window
  | WindowInHandWindow InvestigatorId Window
  | WindowNonFast
  | WindowWhenActAdvance ActId
  | WindowWhenAgendaAdvance AgendaId
  | WindowWhenAllDrawEncounterCard
  | WindowWhenAmongSearchedCards InvestigatorId
  | WindowWhenChosenRandomLocation LocationId
  | WindowWhenDealtDamage Source Target
  | WindowWhenDealtHorror Source Target
  | WindowWhenDefeated Source
  | WindowWhenDiscoverClues InvestigatorId LocationId
  | WindowWhenDrawNonPerilTreachery InvestigatorId TreacheryId
  | WindowWhenDrawToken InvestigatorId Token
  | WindowWhenDrawTreachery InvestigatorId
  | WindowWhenEnemyAttacks InvestigatorId
  | WindowWhenEnemyDefeated InvestigatorId
  | WindowWhenEnemyEvaded InvestigatorId
  | WindowWhenEnemySpawns LocationId [Trait]
  | WindowWhenEnterPlay Target
  | WindowWhenLocationLeavesPlay LocationId
  | WindowWhenPlayCard InvestigatorId CardId
  | WindowWhenRevealToken InvestigatorId Token
  | WindowWhenRevealTokenWithNegativeModifier InvestigatorId TokenId
  | WindowWhenSkillTest SkillType
  | WindowWhenSuccessfulAttackEnemy InvestigatorId EnemyId
  | WindowWhenSuccessfulInvestigation InvestigatorId LocationId
  | WindowWhenTurnBegins InvestigatorId
  | WindowWhenWouldFailSkillTest InvestigatorId
  | WindowWhenWouldLeave InvestigatorId LocationId
  | WindowWhenWouldReady Target
  | WindowWhenWouldRevealChaosToken Source InvestigatorId
  | WindowWhenWouldTakeDamage Source Target
  | WindowWhenWouldTakeHorror Source Target
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMatcher
  = AfterEnemyDefeated Who WindowEnemyMatcher
  | AfterEnemyEvaded Who WindowEnemyMatcher
  | AfterSkillTest Who WindowSkillTestMatcher
  | WhenWouldFailSkillTest Who WindowSkillTestMatcher
  | AfterRevealToken Who WindowTokenMatcher
  | AfterTurnBegins Who
  | DuringTurn Who
  | DuringFast
  | PhaseBegins WindowPhaseMatcher
  | WhenAllDrawEncounterCard
  | WhenEnemyAttacks Who WindowEnemyMatcher
  | WhenEnemySpawns Where WindowEnemyMatcher
  | WhenEnemyDefeated Who WindowEnemyMatcher
  | WhenDrawTreachery Who WindowTreacheryMatcher
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowPhaseMatcher = AnyPhase
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowTreacheryMatcher = AnyTreachery | TreacheryWithoutKeyword Keyword
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowTokenMatcher = TokenWithNegativeModifier
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowEnemyMatcher
  = EnemyMatchingAll [WindowEnemyMatcher]
  | EnemyAt Where
  | NonWeaknessEnemy
  | EnemyWithoutTrait Trait
  | EnemyWithTrait Trait
  | EnemyEngagedWith Who
  | AnyEnemy
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowSkillTestMatcher = FailedByNoMoreThan Int | AttackingEnemy WindowEnemyMatcher | WhileInvestigating WindowLocationMatcher | AnySkillTest | SkillTestMatchingAll [WindowSkillTestMatcher]
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Where = WindowLocationMatcher

data WindowLocationMatcher = YourLocation | ConnectedLocation | Anywhere
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Who = WindowInvestigatorMatcher

data WindowInvestigatorMatcher = You | AnotherInvestigator | InvestigatorAtYourLocation | InvestigatorAtAConnectedLocation | Anyone
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
