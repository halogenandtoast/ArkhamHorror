{-# LANGUAGE TemplateHaskell #-}

module Arkham.Window where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Action (Action)
import Arkham.Agenda.AdvancementReason (AgendaAdvancementReason)
import Arkham.Asset.Uses
import Arkham.Attack.Types
import Arkham.Card (Card)
import Arkham.ChaosToken (ChaosToken)
import Arkham.Damage
import Arkham.DamageEffect (DamageEffect)
import Arkham.Deck
import Arkham.DefeatedBy
import Arkham.Id
import Arkham.Matcher (LocationMatcher, MovesVia)
import Arkham.Phase (Phase)
import Arkham.ScenarioLogKey
import {-# SOURCE #-} Arkham.SkillTest.Base
import Arkham.SkillTest.Step
import Arkham.SkillTest.Type
import Arkham.Source (Source)
import Arkham.Strategy (DamageStrategy)
import Arkham.Target (Target)
import Arkham.Timing (Timing)
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Data.Aeson.TH
import GHC.Records

data Result b a = Success a | Failure b
  deriving stock (Show, Eq, Ord)

data Window = Window
  { windowTiming :: Timing
  , windowType :: WindowType
  , windowBatchId :: Maybe BatchId
  }
  deriving stock (Show, Eq)

replaceWindowType :: WindowType -> Window -> Window
replaceWindowType wType window = window {windowType = wType}

instance HasField "timing" Window Timing where
  getField = windowTiming

instance HasField "kind" Window WindowType where
  getField = windowType

mkWindow :: Timing -> WindowType -> Window
mkWindow timing windowType = Window timing windowType Nothing

mkWhen :: WindowType -> Window
mkWhen windowType = Window #when windowType Nothing

mkAtIf :: WindowType -> Window
mkAtIf windowType = Window #at windowType Nothing

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

pattern PlacedDoom :: Source -> Target -> Int -> WindowType
pattern PlacedDoom source target n <- PlacedToken source target Doom n
  where
    PlacedDoom source target n = PlacedToken source target Doom n

pattern PlacedClues :: Source -> Target -> Int -> WindowType
pattern PlacedClues source target n <- PlacedToken source target Clue n
  where
    PlacedClues source target n = PlacedToken source target Clue n

pattern PlacedResources :: Source -> Target -> Int -> WindowType
pattern PlacedResources source target n <- PlacedToken source target Token.Resource n
  where
    PlacedResources source target n = PlacedToken source target Token.Resource n

pattern PlacedHorror :: Source -> Target -> Int -> WindowType
pattern PlacedHorror source target n <- PlacedToken source target Horror n
  where
    PlacedHorror source target n = PlacedToken source target Horror n

pattern PlacedDamage :: Source -> Target -> Int -> WindowType
pattern PlacedDamage source target n <- PlacedToken source target Damage n
  where
    PlacedDamage source target n = PlacedToken source target Damage n

data IsDirect = IsDirect | IsNonDirect
  deriving stock (Show, Eq)

data WindowType
  = AttemptToEvadeEnemy InvestigatorId EnemyId
  | AttachCard (Maybe InvestigatorId) Card Target
  | ActAdvance ActId
  | ActivateAbility InvestigatorId [Window] Ability
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
  | AtBeginningOfRound
  | AtEndOfRound
  | GameBegins
  | SkillTestStep SkillTestStep
  | ChosenRandomLocation LocationId
  | CommittedCard InvestigatorId Card
  | CommittedCards InvestigatorId [Card]
  | DealtDamage Source DamageEffect Target Int
  | DealtExcessDamage Source DamageEffect Target Int
  | DealtHorror Source Target Int
  | DeckHasNoCards InvestigatorId
  | EncounterDeckRunsOutOfCards
  | Discarded (Maybe InvestigatorId) Source Card
  | DiscoverClues InvestigatorId LocationId Source Int
  | DiscoveringLastClue InvestigatorId LocationId
  | SuccessfullyInvestigateWithNoClues InvestigatorId LocationId
  | WouldDrawCard InvestigatorId DeckSignifier
  | DrawCard InvestigatorId Card DeckSignifier
  | DrawCards InvestigatorId [Card]
  | DrawChaosToken InvestigatorId ChaosToken
  | DrawingStartingHand InvestigatorId
  | DuringTurn InvestigatorId
  | EndOfGame
  | EndTurn InvestigatorId
  | TreacheryEntersPlay TreacheryId
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
  | Exhausts Target
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
  | InvestigatorResigned InvestigatorId
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
  | PlacedToken Source Target Token Int
  | SpentToken Source Target Token Int
  | LostResources InvestigatorId Source Int
  | LostActions InvestigatorId Source Int
  | WouldPlaceDoom Source Target Int
  | DeckWouldRunOutOfCards InvestigatorId
  | DeckRanOutOfCards InvestigatorId
  | WouldSearchDeck InvestigatorId DeckSignifier
  | WouldLookAtDeck InvestigatorId DeckSignifier
  | SearchedDeck InvestigatorId DeckSignifier
  | LookedAtDeck InvestigatorId DeckSignifier
  | PlacedBreaches Target -- BEGIN Breaches
  | PlacedBreach Target
  | WouldPlaceBreaches Target
  | WouldPlaceBreach Target
  | RemovedBreaches Target
  | RemovedBreach Target
  | WouldRemoveBreaches Target
  | WouldRemoveBreach Target -- END Breaches
  | WouldPayCardCost InvestigatorId ActiveCostId BatchId Card
  | PlayCard InvestigatorId Card
  | PlayEventDiscarding InvestigatorId EventId
  | PutLocationIntoPlay InvestigatorId LocationId
  | RevealLocation InvestigatorId LocationId
  | FlipLocation InvestigatorId LocationId
  | RevealChaosToken InvestigatorId ChaosToken
  | TokensWouldBeRemovedFromChaosBag [ChaosToken]
  | ResolvesChaosToken InvestigatorId ChaosToken
  | IgnoreChaosToken InvestigatorId ChaosToken
  | CancelChaosToken InvestigatorId ChaosToken
  | RevealChaosTokenEffect InvestigatorId ChaosToken EffectId
  | RevealChaosTokenEventEffect InvestigatorId [ChaosToken] EventId
  | RevealChaosTokenAssetAbilityEffect InvestigatorId [ChaosToken] AssetId
  | RevealChaosTokenWithNegativeModifier InvestigatorId ChaosToken
  | WouldPerformRevelationSkillTest InvestigatorId
  | SpentUses InvestigatorId Source AssetId UseType Int
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
  | EntityDiscarded Source Target
  | WouldBeShuffledIntoDeck DeckSignifier Card
  | WouldDrawEncounterCard InvestigatorId Phase
  | WouldFailSkillTest InvestigatorId
  | WouldPassSkillTest InvestigatorId
  | WouldReady Target
  | Readies Target
  | WouldRevealChaosToken Source InvestigatorId
  | WouldTakeDamage Source Target Int DamageStrategy
  | WouldTakeDamageOrHorror Source Target Int Int
  | WouldTakeHorror Source Target Int
  | Explored InvestigatorId (Result Card LocationId)
  | AttemptExplore InvestigatorId
  | EnemiesAttackStep
  | AddingToCurrentDepth
  | EntersThreatArea InvestigatorId Card
  | CancelledOrIgnoredCardOrGameEffect Source -- Diana Stanley
  | ScenarioCountIncremented ScenarioCountKey
  | -- used to avoid checking a window
    DoNotCheckWindow
  deriving stock (Show, Eq)

$( do
    isDirect <- deriveJSON defaultOptions ''IsDirect
    result <- deriveJSON defaultOptions ''Result
    windowType <- deriveJSON defaultOptions ''WindowType
    window <- deriveJSON defaultOptions ''Window
    pure $ concat [isDirect, result, windowType, window]
 )
