{-# LANGUAGE TemplateHaskell #-}

module Arkham.Window where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.Action (Action)
import Arkham.Agenda.AdvancementReason (AgendaAdvancementReason)
import Arkham.Asset.Uses
import Arkham.Attack.Types
import Arkham.Card (Card, CardId)
import Arkham.ChaosToken.Types (ChaosToken, ChaosTokenFace)
import Arkham.Damage
import Arkham.DamageEffect (DamageEffect)
import Arkham.Deck
import Arkham.DefeatedBy
import Arkham.Id
import Arkham.Key
import Arkham.Location.FloodLevel
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
import Data.UUID qualified as UUID
import GHC.Records

data Result b a = Success a | Failure b
  deriving stock (Show, Eq, Ord, Data)

data Window = Window
  { windowTiming :: Timing
  , windowType :: WindowType
  , windowBatchId :: Maybe BatchId
  }
  deriving stock (Show, Eq, Data)

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
  deriving stock (Show, Eq, Data)

data CardPlay = CardPlay
  { cardPlayedCard :: Card
  , cardPlayedNeedsAction :: Bool
  }
  deriving stock (Show, Eq, Data)

instance HasField "card" CardPlay Card where
  getField = cardPlayedCard

instance HasField "needsAction" CardPlay Bool where
  getField = cardPlayedNeedsAction

data WindowType
  = AttemptToEvadeEnemy SkillTestId InvestigatorId EnemyId
  | ResolvingRevelation InvestigatorId TreacheryId
  | VehicleLeaves AssetId LocationId
  | VehicleEnters AssetId LocationId
  | FloodLevelChanged LocationId FloodLevel FloodLevel
  | FloodLevelIncreased LocationMatcher
  | FirstTimeParleyingThisRound InvestigatorId
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
  | CommittingCardsFromHandToSkillTestStep InvestigatorId
  | DealtDamage Source DamageEffect Target Int
  | DealtExcessDamage Source DamageEffect Target Int
  | DealtHorror Source Target Int
  | DeckHasNoCards InvestigatorId
  | EncounterDeckRunsOutOfCards
  | Discarded (Maybe InvestigatorId) Source Card
  | DiscardedFromHand InvestigatorId Source Card
  | WouldDiscardFromHand InvestigatorId Source
  | DiscoverClues InvestigatorId LocationId Source Int
  | WouldDiscoverClues InvestigatorId LocationId Source Int
  | SpentClues InvestigatorId Int
  | DiscoveringLastClue InvestigatorId LocationId
  | SuccessfullyInvestigateWithNoClues InvestigatorId LocationId
  | WouldDrawCard InvestigatorId DeckSignifier
  | DrawCard InvestigatorId Card DeckSignifier
  | DrawCards InvestigatorId [Card]
  | DrawChaosToken InvestigatorId ChaosToken
  | DrawingStartingHand InvestigatorId
  | DuringTurn InvestigatorId
  | EndOfGame
  | WouldEndTurn InvestigatorId
  | EndTurn InvestigatorId
  | TreacheryEntersPlay TreacheryId
  | EnemyAttacked InvestigatorId Source EnemyId
  | EnemyAttacks EnemyAttackDetails
  | EnemyAttacksEvenIfCancelled EnemyAttackDetails
  | EnemyAttemptsToSpawnAt EnemyId LocationMatcher
  | EnemyDefeated (Maybe InvestigatorId) DefeatedBy EnemyId
  | IfEnemyDefeated (Maybe InvestigatorId) DefeatedBy EnemyId
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
  | GainsResources InvestigatorId Source Int
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
  | WouldMoveFromHunter EnemyId
  | EnemyMovesTo LocationId MovesVia EnemyId
  | EnemyMoves EnemyId LocationId
  | HuntersMoveStep
  | Moves InvestigatorId Source (Maybe LocationId) LocationId
  | WouldMove InvestigatorId Source LocationId LocationId
  | NonFast
  | PassInvestigationSkillTest InvestigatorId LocationId Int
  | PassSkillTest (Maybe Action) Source InvestigatorId Int
  | PerformAction InvestigatorId Action
  | PerformedSameTypeOfAction InvestigatorId [Action]
  | PerformedDifferentTypesOfActionsInARow InvestigatorId Int [Action]
  | PhaseBegins Phase
  | PhaseEnds Phase
  | PlaceUnderneath Target Card
  | PlacedToken Source Target Token Int
  | PlacedDoomCounterOnTargetWithNoDoom Source Target Int
  | InvestigatorPlacedFromTheirPool InvestigatorId Source Target Token Int
  | SpentToken Source Target Token Int
  | AttackOrEffectSpentLastUse Source Target Token
  | LostResources InvestigatorId Source Int
  | LostActions InvestigatorId Source Int
  | WouldPlaceDoom Source Target Int
  | WouldPlaceClueOnLocation InvestigatorId LocationId Source Int
  | TakeControlOfClues InvestigatorId Source Int
  | TakeControlOfKey InvestigatorId ArkhamKey
  | WouldAddChaosTokensToChaosBag (Maybe InvestigatorId) [ChaosTokenFace]
  | DeckWouldRunOutOfCards InvestigatorId
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
  | PlayCard InvestigatorId CardPlay
  | PlayEventDiscarding InvestigatorId EventId
  | PlayEvent InvestigatorId EventId
  | PutLocationIntoPlay InvestigatorId LocationId
  | LocationEntersPlay LocationId
  | RevealLocation InvestigatorId LocationId
  | FlipLocation InvestigatorId LocationId
  | RevealChaosToken InvestigatorId ChaosToken
  | RevealChaosTokensDuringSkillTest InvestigatorId SkillTest [ChaosToken]
  | TokensWouldBeRemovedFromChaosBag [ChaosToken]
  | ResolvesTreachery InvestigatorId TreacheryId
  | ResolvesChaosToken InvestigatorId ChaosToken
  | ChaosTokenSealed InvestigatorId ChaosToken
  | IgnoreChaosToken InvestigatorId ChaosToken
  | CancelChaosToken InvestigatorId ChaosToken
  | RevealChaosTokenEffect InvestigatorId ChaosToken EffectId
  | RevealChaosTokenEventEffect InvestigatorId [ChaosToken] EventId
  | RevealChaosTokenTreacheryEffect InvestigatorId [ChaosToken] TreacheryId
  | RevealChaosTokenAssetAbilityEffect InvestigatorId [ChaosToken] AssetId
  | RevealChaosTokenWithNegativeModifier InvestigatorId ChaosToken
  | WouldPerformRevelationSkillTest InvestigatorId SkillTestId
  | SpentUses InvestigatorId Source AssetId UseType Int
  | SkillTest SkillTestType
  | SkillTestEnded SkillTest
  | SuccessfulAttackEnemy InvestigatorId Source EnemyId Int
  | SuccessfulEvadeEnemy InvestigatorId EnemyId Int
  | SuccessfulInvestigation InvestigatorId LocationId
  | SuccessfulParley InvestigatorId
  | TakeDamage Source DamageEffect Target Int
  | TakeHorror Source Target
  | TookControlOfAsset InvestigatorId AssetId
  | TurnBegins InvestigatorId
  | TurnEnds InvestigatorId
  | WouldBeDiscarded Target
  | EntityDiscarded Source Target
  | WouldBeShuffledIntoDeck DeckSignifier Card
  | WouldDrawEncounterCard InvestigatorId Phase
  | WouldFailSkillTest InvestigatorId Int
  | WouldPassSkillTest InvestigatorId Int
  | WouldReady Target
  | Readies Target
  | WouldRevealChaosToken Source InvestigatorId
  | WouldRevealChaosTokens Source InvestigatorId
  | WouldTakeDamage Source Target Int DamageStrategy
  | WouldTakeDamageOrHorror Source Target Int Int
  | WouldTakeHorror Source Target Int
  | Explored InvestigatorId (Maybe LocationId) (Result Card LocationId)
  | AttemptExplore InvestigatorId
  | EnemiesAttackStep
  | AddingToCurrentDepth
  | EntersThreatArea InvestigatorId Card
  | CancelledOrIgnoredCardOrGameEffect Source (Maybe CardId)-- Diana Stanley
  | ScenarioCountIncremented ScenarioCountKey
  | IncreasedAlarmLevel InvestigatorId
  | ScenarioEvent Text (Maybe InvestigatorId) Value
  | -- used to avoid checking a window
    DoNotCheckWindow
  deriving stock (Show, Eq, Data)

mconcat
  [ deriveJSON defaultOptions ''IsDirect
  , deriveJSON defaultOptions ''CardPlay
  , deriveJSON defaultOptions ''Result
  , deriveToJSON defaultOptions ''WindowType
  , [d|
      instance FromJSON WindowType where
        parseJSON = withObject "WindowType" \o -> do
          tag :: Text <- o .: "tag"
          case tag of
            "Explored" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (a, b) -> pure $ Explored a Nothing b
                Right (a, b, c) -> pure $ Explored a b c
            "DeckRanOutOfCards" -> DeckHasNoCards <$> o .: "contents"
            "ScenarioEvent" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (a, b) -> pure $ ScenarioEvent a Nothing b
                Right (a, b, c) -> pure $ ScenarioEvent a b c
            "AttemptToEvadeEnemy" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (i, e) -> pure $ AttemptToEvadeEnemy (SkillTestId UUID.nil) i e
                Right (sid, i, e) -> pure $ AttemptToEvadeEnemy sid i e
            "PlayCard" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (i, c) -> pure $ PlayCard i (CardPlay c True)
                Right (i, cp) -> pure $ PlayCard i cp
            "WouldAddChaosTokensToChaosBag" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left cs -> pure $ WouldAddChaosTokensToChaosBag Nothing cs
                Right (i, cs) -> pure $ WouldAddChaosTokensToChaosBag i cs
            _ -> $(mkParseJSON defaultOptions ''WindowType) (Object o)
      |]
  , deriveJSON defaultOptions ''Window
  , makePrisms ''WindowType
  ]
