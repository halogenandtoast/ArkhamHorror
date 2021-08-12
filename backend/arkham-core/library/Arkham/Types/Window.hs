module Arkham.Types.Window
  ( module Arkham.Types.Window
  , module X
  ) where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Card
import Arkham.Types.Id
import Arkham.Types.Phase
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing as X
import Arkham.Types.Token

data Window = Window
  { windowTiming :: Timing
  , windowType :: WindowType
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

after :: WindowType -> Window
after = Window After

before :: WindowType -> Window
before = Window Before

when :: WindowType -> Window
when = Window When

data WindowType
  = DiscoveringClues InvestigatorId LocationId -- name conflict resolution
  | DrawCard InvestigatorId Card
  | DrawingStartingHand InvestigatorId
  | CommittedCards InvestigatorId [Card]
  | EndTurn InvestigatorId
  | EnemyDefeated InvestigatorId EnemyId
  | EnemyEngageInvestigator InvestigatorId EnemyId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyLeaves EnemyId LocationId
  | FailAttackEnemy InvestigatorId EnemyId
  | FailInvestigationSkillTest InvestigatorId Int
  | FailSkillTest InvestigatorId Int
  | FailSkillTestAtOrLess InvestigatorId Int
  | Leaving InvestigatorId LocationId
  | MoveFromHunter EnemyId
  | Entering InvestigatorId LocationId
  | PassSkillTest (Maybe Action) Source InvestigatorId Int
  | PlayCard InvestigatorId Card
  | PutLocationIntoPlay InvestigatorId
  | RevealLocation InvestigatorId
  | SuccessfulAttackEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation InvestigatorId LocationId
  | TurnBegins InvestigatorId
  | AnyPhaseBegins
  | PhaseBegins Phase
  | PhaseEnds Phase
  | AtEndOfRound
  | DuringTurn InvestigatorId
  | FastPlayerWindow
  | InDiscardWindow InvestigatorId Window
  | InHandWindow InvestigatorId Window
  | NonFast
  | ActAdvance ActId
  | AgendaAdvance AgendaId
  | AllDrawEncounterCard
  | AmongSearchedCards InvestigatorId
  | ChosenRandomLocation LocationId
  | DealtDamage Source Target
  | DealtHorror Source Target
  | Defeated Source
  | DiscoverClues InvestigatorId LocationId Int
  | WouldDrawEncounterCard InvestigatorId
  | DrawToken InvestigatorId Token
  | EnemyAttacks InvestigatorId EnemyId
  | EnemySpawns EnemyId LocationId
  | EnterPlay Target
  | LocationLeavesPlay LocationId
  | RevealToken InvestigatorId Token
  | RevealTokenWithNegativeModifier InvestigatorId Token
  | SkillTest SkillType
  | WouldFailSkillTest InvestigatorId
  | WouldLeave InvestigatorId LocationId
  | TargetReadies Target
  | WouldRevealChaosToken Source InvestigatorId
  | WouldTakeDamage Source Target
  | WouldTakeHorror Source Target
  | WouldTakeDamageOrHorror Source Target Int Int
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
