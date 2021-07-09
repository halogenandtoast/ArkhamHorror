module Arkham.Types.Window where

import Arkham.Prelude

import Arkham.Types.ActId
import Arkham.Types.Action
import Arkham.Types.AgendaId
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId

data Window = Window
  { windowSource :: Maybe Source
  , windowTarget :: Maybe Target
  , windowType :: WindowType
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowType
  = AfterDiscoveringClues Who Where -- name conflict resolution
  | AfterDrawCard Who CardId
  | AfterEndTurn Who
  | AfterEnemyDefeated Who EnemyId
  | AfterEnemyDefeatedOfType Who Trait
  | AfterEnemyEngageInvestigator Who EnemyId
  | AfterEnemyEvaded Who EnemyId
  | AfterFailInvestigationSkillTest Who Int
  | AfterFailSkillTest Who Int
  | AfterFailSkillTestAtOrLess Who Int
  | AfterLeaving Who LocationId
  | AfterPassSkillTest (Maybe Action) Source Who Int
  | AfterPlayCard Who [Trait]
  | AfterPutLocationIntoPlay Who
  | AfterRevealLocation Who
  | AfterSuccessfulAttackEnemy Who EnemyId
  | AfterSuccessfulInvestigation Who Where
  | AfterTurnBegins Who
  | AnyPhaseBegins
  | AtEndOfRound
  | DuringTurn Who
  | FastPlayerWindow
  | InDiscardWindow InvestigatorId Window
  | InHandWindow InvestigatorId Window
  | NonFast
  | WhenActAdvance ActId
  | WhenAgendaAdvance AgendaId
  | WhenAllDrawEncounterCard
  | WhenAmongSearchedCards Who
  | WhenChosenRandomLocation LocationId
  | WhenDealtDamage Source Target
  | WhenDealtHorror Source Target
  | WhenDefeated Source
  | WhenDiscoverClues Who Where
  | WhenDrawNonPerilTreachery Who TreacheryId
  | WhenDrawToken Who Token
  | WhenDrawTreachery Who
  | WhenEnemyAttacks Who
  | WhenEnemyDefeated Who
  | WhenEnemyEvaded Who
  | WhenEnemySpawns Where [Trait]
  | WhenEnterPlay Target
  | WhenLocationLeavesPlay LocationId
  | WhenPlayCard Who CardId
  | WhenRevealToken Who Token
  | WhenRevealTokenWithNegativeModifier Who TokenId
  | WhenSkillTest SkillType
  | WhenSuccessfulAttackEnemy Who EnemyId
  | WhenSuccessfulInvestigation Who Where
  | WhenTurnBegins Who
  | WhenWouldFailSkillTest Who
  | WhenWouldLeave Who LocationId
  | WhenWouldReady Target
  | WhenWouldRevealChaosToken Source Who
  | WhenWouldTakeDamage Source Target
  | WhenWouldTakeHorror Source Target
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Where = YourLocation | ConnectedLocation | LocationInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Who = You | InvestigatorAtYourLocation | InvestigatorAtAConnectedLocation | InvestigatorInGame
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
