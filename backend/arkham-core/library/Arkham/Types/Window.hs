module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.Action (Action)
import Arkham.Types.Agenda.AdvancementReason
import Arkham.Types.Card (Card)
import Arkham.Types.Deck
import Arkham.Types.Id
import Arkham.Types.Matcher (LocationMatcher)
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
  | AgendaWouldAdvance AgendaAdvancementReason AgendaId
  | AllUndefeatedInvestigatorsResigned
  | AllDrawEncounterCard
  | AmongSearchedCards InvestigatorId
  | AnyPhaseBegins
  | AtEndOfRound
  | EndOfGame
  | ChosenRandomLocation LocationId
  | CommittedCards InvestigatorId [Card]
  | DealtDamage Source Target
  | DealtHorror Source Target
  | Defeated Source
  | DiscoverClues InvestigatorId LocationId Int
  | GainsClues InvestigatorId Int
  | DiscoveringLastClue InvestigatorId LocationId
  | DrawCard InvestigatorId Card DeckSignifier
  | Discarded InvestigatorId Card
  | WouldBeDiscarded Target
  | DrawToken InvestigatorId Token
  | DrawingStartingHand InvestigatorId
  | DuringTurn InvestigatorId
  | EndTurn InvestigatorId
  | InvestigatorDefeated Source InvestigatorId
  | InvestigatorEliminated InvestigatorId
  | AssetDefeated AssetId
  | TookControlOfAsset InvestigatorId AssetId
  | EnemyAttacks InvestigatorId EnemyId
  | EnemyAttacked InvestigatorId Source EnemyId
  | EnemyDefeated InvestigatorId EnemyId
  | EnemyWouldBeDefeated EnemyId
  | EnemyEngaged InvestigatorId EnemyId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyAttemptsToSpawnAt EnemyId LocationMatcher
  | EnemySpawns EnemyId LocationId
  | EnemyEnters EnemyId LocationId
  | EnemyLeaves EnemyId LocationId
  | EnterPlay Target
  | Entering InvestigatorId LocationId
  | FailAttackEnemy InvestigatorId EnemyId Int
  | FailEvadeEnemy InvestigatorId EnemyId Int
  | FailInvestigationSkillTest InvestigatorId LocationId Int
  | FailSkillTest InvestigatorId Int
  | FailSkillTestAtOrLess InvestigatorId Int
  | FastPlayerWindow
  | InDiscardWindow InvestigatorId Window
  | InHandWindow InvestigatorId Window
  | Moves InvestigatorId LocationId LocationId
  | Leaving InvestigatorId LocationId
  | LeavePlay Target
  | MovedFromHunter EnemyId
  | MovedBy Source LocationId InvestigatorId
  | NonFast
  | PassSkillTest (Maybe Action) Source InvestigatorId Int
  | PassInvestigationSkillTest InvestigatorId LocationId Int
  | PhaseBegins Phase
  | PhaseEnds Phase
  | PlacedHorror InvestigatorId Int
  | PlacedDamage InvestigatorId Int
  | PlacedClues Target Int
  | PlayCard InvestigatorId Card
  | PutLocationIntoPlay InvestigatorId LocationId
  | RevealLocation InvestigatorId LocationId
  | RevealToken InvestigatorId Token
  | RevealTokenWithNegativeModifier InvestigatorId Token
  | SkillTest SkillType
  | SuccessfulAttackEnemy InvestigatorId EnemyId Int
  | SuccessfulEvadeEnemy InvestigatorId EnemyId Int
  | SuccessfulInvestigation InvestigatorId LocationId
  | TurnBegins InvestigatorId
  | TurnEnds InvestigatorId
  | DeckHasNoCards InvestigatorId
  | WouldDrawEncounterCard InvestigatorId
  | WouldFailSkillTest InvestigatorId
  | WouldReady Target
  | WouldRevealChaosToken Source InvestigatorId
  | WouldTakeDamage Source Target
  | WouldTakeDamageOrHorror Source Target Int Int
  | WouldTakeHorror Source Target
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
