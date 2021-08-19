module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.Action (Action)
import Arkham.Types.Card (Card)
import Arkham.Types.Id
import Arkham.Types.Phase (Phase)
import Arkham.Types.SkillType (SkillType)
import Arkham.Types.Source (Source)
import Arkham.Types.Target (Target)
import Arkham.Types.Timing (Timing)
import Arkham.Types.Token (Token)

data Window = Window
  { windowTiming :: Timing
  , windowType :: WindowType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowType
  = ActAdvance ActId
  | AgendaAdvance AgendaId
  | AllDrawEncounterCard
  | AmongSearchedCards InvestigatorId
  | AnyPhaseBegins
  | AtEndOfRound
  | ChosenRandomLocation LocationId
  | CommitedCard InvestigatorId Card
  | DealtDamage Source Target
  | DealtHorror Source Target
  | Defeated Source
  | DiscoverClues InvestigatorId LocationId Int
  | DiscoveringLastClue InvestigatorId LocationId
  | DrawCard InvestigatorId Card
  | DrawToken InvestigatorId Token
  | DrawingStartingHand InvestigatorId
  | DuringTurn InvestigatorId
  | EndTurn InvestigatorId
  | EnemyAttacks InvestigatorId EnemyId
  | EnemyDefeated InvestigatorId EnemyId
  | EnemyEngaged InvestigatorId EnemyId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemySpawns EnemyId LocationId
  | EnterPlay Target
  | Entering InvestigatorId LocationId
  | FailAttackEnemy InvestigatorId EnemyId Int
  | FailInvestigationSkillTest InvestigatorId Int
  | FailSkillTest InvestigatorId Int
  | FailSkillTestAtOrLess InvestigatorId Int
  | FastPlayerWindow
  | InDiscardWindow InvestigatorId Window
  | InHandWindow InvestigatorId Window
  | Leaving InvestigatorId LocationId
  | LocationLeavesPlay LocationId
  | MoveFromHunter EnemyId
  | NonFast
  | PassSkillTest (Maybe Action) Source InvestigatorId Int
  | PhaseBegins Phase
  | PhaseEnds Phase
  | PlacedHorror InvestigatorId Int
  | PlacedDamage InvestigatorId Int
  | PlayCard InvestigatorId Card
  | PutLocationIntoPlay InvestigatorId LocationId
  | RevealLocation InvestigatorId LocationId
  | RevealToken InvestigatorId Token
  | RevealTokenWithNegativeModifier InvestigatorId Token
  | SkillTest SkillType
  | SuccessfulAttackEnemy InvestigatorId EnemyId Int
  | SuccessfulInvestigation InvestigatorId LocationId
  | TurnBegins InvestigatorId
  | TurnEnds InvestigatorId
  | WouldDrawEncounterCard InvestigatorId
  | WouldFailSkillTest InvestigatorId
  | WouldLeave InvestigatorId LocationId
  | WouldReady Target
  | WouldRevealChaosToken Source InvestigatorId
  | WouldTakeDamage Source Target
  | WouldTakeDamageOrHorror Source Target Int Int
  | WouldTakeHorror Source Target
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
