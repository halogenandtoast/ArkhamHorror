{-# LANGUAGE TemplateHaskell #-}

module Arkham.Window where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Action (Action)
import Arkham.Agenda.AdvancementReason (AgendaAdvancementReason)
import Arkham.Attack
import Arkham.Card (Card)
import Arkham.ChaosToken (ChaosToken)
import Arkham.Damage
import Arkham.DamageEffect (DamageEffect)
import Arkham.Deck
import Arkham.DefeatedBy
import Arkham.Id
import Arkham.Matcher (LocationMatcher, MovesVia)
import Arkham.Phase (Phase)
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.Source (Source)
import Arkham.Target (Target)
import Arkham.Timing (Timing)
import Arkham.Timing qualified as Timing
import Data.Aeson.TH

data Result b a = Success a | Failure b
  deriving stock (Show, Eq, Ord)

data Window = Window
  { windowTiming :: Timing
  , windowType :: WindowType
  , windowBatchId :: Maybe BatchId
  }
  deriving stock (Show, Eq, Ord)

mkWindow :: Timing -> WindowType -> Window
mkWindow timing windowType = Window timing windowType Nothing

mkWhen :: WindowType -> Window
mkWhen windowType = Window #when windowType Nothing

mkAfter :: WindowType -> Window
mkAfter windowType = Window #after windowType Nothing

windowTypes :: [Window] -> [WindowType]
windowTypes = map windowType

getBatchId :: [Window] -> BatchId
getBatchId ((windowBatchId -> Just batchId) : _) = batchId
getBatchId (_ : rest) = getBatchId rest
getBatchId [] = error "No batch id found"

duringTurnWindow :: InvestigatorId -> Window
duringTurnWindow = mkWindow Timing.When . DuringTurn

defaultWindows :: InvestigatorId -> [Window]
defaultWindows iid =
  [ duringTurnWindow iid
  , mkWindow Timing.When NonFast
  , mkWindow Timing.When FastPlayerWindow
  ]

hasEliminatedWindow :: [Window] -> Bool
hasEliminatedWindow = any $ \case
  (windowType -> InvestigatorEliminated {}) -> True
  (windowType -> EndOfGame {}) -> True
  _ -> False

revealedChaosTokens :: [Window] -> [ChaosToken]
revealedChaosTokens [] = []
revealedChaosTokens ((windowType -> RevealChaosToken _ token) : rest) = token : revealedChaosTokens rest
revealedChaosTokens (_ : rest) = revealedChaosTokens rest

data WindowType
  = ActAdvance ActId
  | ActivateAbility InvestigatorId Ability
  | AddedToVictory Card
  | AgendaAdvance AgendaId
  | AgendaWouldAdvance AgendaAdvancementReason AgendaId
  | AllDrawEncounterCard
  | AfterCheckDoomThreshold
  | AllUndefeatedInvestigatorsResigned
  | AmongSearchedCards BatchId InvestigatorId
  | AnyPhaseBegins
  | AssetDefeated AssetId DefeatedBy
  | AssignedHorror Source InvestigatorId [Target]
  | AtEndOfRound
  | GameBegins
  | ChosenRandomLocation LocationId
  | CommittedCard InvestigatorId Card
  | CommittedCards InvestigatorId [Card]
  | DealtDamage Source DamageEffect Target Int
  | DealtExcessDamage Source DamageEffect Target Int
  | DealtHorror Source Target Int
  | DeckHasNoCards InvestigatorId
  | EncounterDeckRunsOutOfCards
  | Discarded InvestigatorId Source Card
  | DiscoverClues InvestigatorId LocationId Source Int
  | DiscoveringLastClue InvestigatorId LocationId
  | SuccessfullyInvestigateWithNoClues InvestigatorId LocationId
  | DrawCard InvestigatorId Card DeckSignifier
  | DrawCards InvestigatorId [Card]
  | DrawChaosToken InvestigatorId ChaosToken
  | DrawingStartingHand InvestigatorId
  | DuringTurn InvestigatorId
  | EndOfGame
  | EndTurn InvestigatorId
  | EnemyAttacked InvestigatorId Source EnemyId
  | EnemyAttacks EnemyAttackDetails
  | EnemyAttacksEvenIfCancelled EnemyAttackDetails
  | EnemyAttemptsToSpawnAt EnemyId LocationMatcher
  | EnemyDefeated (Maybe InvestigatorId) DefeatedBy EnemyId
  | EnemyEngaged InvestigatorId EnemyId
  | EnemyEnters EnemyId LocationId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyLeaves EnemyId LocationId
  | EnemyWouldSpawnAt EnemyId LocationId
  | EnemySpawns EnemyId LocationId
  | EnemyWouldAttack EnemyAttackDetails
  | EnemyWouldBeDefeated EnemyId
  | EnterPlay Target
  | Entering InvestigatorId LocationId
  | FailAttackEnemy InvestigatorId EnemyId Int
  | FailEvadeEnemy InvestigatorId EnemyId Int
  | FailInvestigationSkillTest InvestigatorId LocationId Int
  | FailSkillTest InvestigatorId Int
  | FailSkillTestAtOrLess InvestigatorId Int
  | FastPlayerWindow
  | GainsClues InvestigatorId Source Int
  | Healed DamageType Target Source Int
  | InDiscardWindow InvestigatorId Window
  | InHandWindow InvestigatorId Window
  | InitiatedSkillTest SkillTest
  | InvestigatorDefeated DefeatedBy InvestigatorId
  | InvestigatorWouldBeDefeated DefeatedBy InvestigatorId
  | InvestigatorEliminated InvestigatorId
  | LastClueRemovedFromAsset AssetId
  | LeavePlay Target
  | Leaving InvestigatorId LocationId
  | MoveAction InvestigatorId LocationId LocationId
  | MovedButBeforeEnemyEngagement InvestigatorId LocationId
  | MovedBy Source LocationId InvestigatorId
  | MovedFromHunter EnemyId
  | EnemyMovesTo LocationId MovesVia EnemyId
  | HuntersMoveStep
  | Moves InvestigatorId Source (Maybe LocationId) LocationId
  | NonFast
  | PassInvestigationSkillTest InvestigatorId LocationId Int
  | PassSkillTest (Maybe Action) Source InvestigatorId Int
  | PerformAction InvestigatorId Action
  | PerformedSameTypeOfAction InvestigatorId [Action]
  | PhaseBegins Phase
  | PhaseEnds Phase
  | PlaceUnderneath Target Card
  | PlacedClues Source Target Int
  | PlacedResources Source Target Int
  | LostResources InvestigatorId Source Int
  | LostActions InvestigatorId Source Int
  | PlacedDamage Source Target Int
  | PlacedHorror Source Target Int
  | PlacedDoom Source Target Int
  | WouldPlaceDoom Source Target Int
  | DeckWouldRunOutOfCards InvestigatorId
  | DeckRanOutOfCards InvestigatorId
  | WouldSearchDeck InvestigatorId DeckSignifier
  | SearchedDeck InvestigatorId DeckSignifier
  | PlacedBreaches Target -- BEGIN Breaches
  | PlacedBreach Target
  | WouldPlaceBreaches Target
  | WouldPlaceBreach Target
  | RemovedBreaches Target
  | RemovedBreach Target
  | WouldRemoveBreaches Target
  | WouldRemoveBreach Target -- END Breaches
  | PlayCard InvestigatorId Card
  | PlayEventDiscarding InvestigatorId EventId
  | PutLocationIntoPlay InvestigatorId LocationId
  | RevealLocation InvestigatorId LocationId
  | FlipLocation InvestigatorId LocationId
  | RevealChaosToken InvestigatorId ChaosToken
  | IgnoreChaosToken InvestigatorId ChaosToken
  | CancelChaosToken InvestigatorId ChaosToken
  | RevealChaosTokenEffect InvestigatorId ChaosToken EffectId
  | RevealChaosTokenEventEffect InvestigatorId [ChaosToken] EventId
  | RevealChaosTokenAssetAbilityEffect InvestigatorId [ChaosToken] AssetId
  | RevealChaosTokenWithNegativeModifier InvestigatorId ChaosToken
  | WouldPerformRevelationSkillTest InvestigatorId
  | SkillTest SkillTestType
  | SkillTestEnded SkillTest
  | SuccessfulAttackEnemy InvestigatorId EnemyId Int
  | SuccessfulEvadeEnemy InvestigatorId EnemyId Int
  | SuccessfulInvestigation InvestigatorId LocationId
  | TakeDamage Source DamageEffect Target Int
  | TakeHorror Source Target
  | TookControlOfAsset InvestigatorId AssetId
  | TurnBegins InvestigatorId
  | TurnEnds InvestigatorId
  | WouldBeDiscarded Target
  | WouldBeShuffledIntoDeck DeckSignifier Card
  | WouldDrawEncounterCard InvestigatorId Phase
  | WouldFailSkillTest InvestigatorId
  | WouldPassSkillTest InvestigatorId
  | WouldReady Target
  | WouldRevealChaosToken Source InvestigatorId
  | WouldTakeDamage Source Target Int
  | WouldTakeDamageOrHorror Source Target Int Int
  | WouldTakeHorror Source Target Int
  | Explored InvestigatorId (Result Card LocationId)
  | AttemptExplore InvestigatorId
  | EnemiesAttackStep
  | AddingToCurrentDepth
  | CancelledOrIgnoredCardOrGameEffect Source -- Diana Stanley
  | -- used to avoid checking a window
    DoNotCheckWindow
  deriving stock (Show, Ord, Eq)

$( do
    result <- deriveJSON defaultOptions ''Result
    windowType <- deriveJSON defaultOptions ''WindowType
    window <- deriveJSON defaultOptions ''Window
    pure $ concat [result, windowType, window]
 )
