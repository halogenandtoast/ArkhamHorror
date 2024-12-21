{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Window where

import Arkham.Agenda.AdvancementReason
import Arkham.Asset.Uses
import Arkham.ChaosToken.Types
import Arkham.Cost.Status
import {-# SOURCE #-} Arkham.Criteria
import Arkham.Damage
import {-# SOURCE #-} Arkham.Matcher.Ability
import Arkham.Matcher.Act
import Arkham.Matcher.Action
import Arkham.Matcher.Agenda
import Arkham.Matcher.Asset
import Arkham.Matcher.Card
import Arkham.Matcher.ChaosToken
import Arkham.Matcher.Counter
import Arkham.Matcher.Damage
import Arkham.Matcher.Enemy
import Arkham.Matcher.Event
import Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Phase
import Arkham.Matcher.SkillTest
import Arkham.Matcher.SkillType
import Arkham.Matcher.Source
import Arkham.Matcher.Target
import Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Step
import Arkham.Timing
import Data.Aeson.TH

data MovesVia = MovedViaHunter | MovedViaOther | MovedViaAny
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

type FromWhere = Where
type ToWhere = Where

data WindowMatcher
  = EnemyDefeated Timing Who DefeatedByMatcher EnemyMatcher
  | VehicleLeaves Timing AssetMatcher LocationMatcher
  | VehicleEnters Timing AssetMatcher LocationMatcher
  | FloodLevelChanged Timing LocationMatcher
  | FloodLevelIncreased Timing LocationMatcher
  | FirstTimeParleyingThisRound Timing Who
  | SpentUses Timing Who SourceMatcher UseType AssetMatcher ValueMatcher
  | AttackOrEffectSpentLastUse Timing SourceMatcher TargetMatcher UseType
  | WouldPayCardCost Timing Who CardMatcher
  | WouldBeShuffledIntoDeck DeckMatcher CardMatcher
  | AddedToVictory Timing CardMatcher
  | PerformAction Timing Who ActionMatcher
  | PerformedSameTypeOfAction Timing Who ActionMatcher
  | DrawingStartingHand Timing Who
  | InvestigatorDefeated Timing DefeatedByMatcher Who
  | InvestigatorWouldBeDefeated Timing DefeatedByMatcher Who
  | InvestigatorWouldTakeDamage Timing Who SourceMatcher DamageTypeMatcher
  | InvestigatorWouldTakeHorror Timing Who SourceMatcher
  | WouldSearchDeck Timing Who DeckMatcher
  | WouldLookAtDeck Timing Who DeckMatcher
  | LookedAtDeck Timing Who DeckMatcher
  | SearchedDeck Timing Who DeckMatcher
  | AmongSearchedCards Who
  | DeckWouldRunOutOfCards Timing Who
  | DeckHasNoCards Timing Who
  | EncounterDeckRunsOutOfCards
  | MovedBy Timing Who SourceMatcher
  | MovedButBeforeEnemyEngagement Timing Who Where
  | MovedFromHunter Timing EnemyMatcher
  | ChosenRandomLocation Timing LocationMatcher
  | PlaceUnderneath Timing TargetMatcher CardMatcher
  | WouldPlaceDoomCounter Timing SourceMatcher TargetMatcher
  | PlacedDoomCounter Timing SourceMatcher TargetMatcher
  | PlacedDoomCounterOnTargetWithNoDoom Timing SourceMatcher TargetMatcher
  | SpentClues Timing InvestigatorMatcher ValueMatcher
  | PlacedToken Timing SourceMatcher TargetMatcher Token
  | InvestigatorPlacedFromTheirPool Timing Who SourceMatcher TargetMatcher Token
  | WouldPlaceBreach Timing TargetMatcher
  | PlacedBreach Timing TargetMatcher
  | PlacedBreaches Timing TargetMatcher
  | WouldRemoveBreach Timing TargetMatcher
  | RemovedBreach Timing TargetMatcher
  | RemovedBreaches Timing TargetMatcher
  | EnemyWouldBeDefeated Timing EnemyMatcher
  | EnemyWouldReady Timing EnemyMatcher
  | EnemyReadies Timing EnemyMatcher
  | EnemyEnters Timing Where EnemyMatcher
  | EnemyLeaves Timing Where EnemyMatcher
  | AgendaAdvances Timing AgendaMatcher
  | ActAdvances Timing ActMatcher
  | AgendaWouldAdvance Timing AgendaAdvancementReason AgendaMatcher
  | AssetDefeated Timing DefeatedByMatcher AssetMatcher
  | AttemptToEvade Timing Who EnemyMatcher
  | AttachCard Timing (Maybe Who) CardMatcher TargetMatcher
  | EnemyEvaded Timing Who EnemyMatcher
  | EnemyEngaged Timing Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | LocationEntersPlay Timing LocationMatcher
  | ResolvesTreachery Timing Who TreacheryMatcher
  | TreacheryEntersPlay Timing TreacheryMatcher
  | AgendaEntersPlay Timing AgendaMatcher
  | AssetEntersPlay Timing AssetMatcher
  | AssetLeavesPlay Timing AssetMatcher
  | AssetDealtDamage Timing SourceMatcher AssetMatcher
  | AssetDealtDamageOrHorror Timing SourceMatcher AssetMatcher
  | LastClueRemovedFromAsset Timing AssetMatcher
  | EnemyDealtDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyDealtExcessDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyTakeDamage Timing DamageEffectMatcher EnemyMatcher ValueMatcher SourceMatcher
  | InvestigatorTakeDamage Timing Who SourceMatcher
  | InvestigatorTakeHorror Timing Who SourceMatcher
  | EnemyLeavesPlay Timing EnemyMatcher
  | LocationLeavesPlay Timing LocationMatcher
  | TookControlOfAsset Timing Who AssetMatcher
  | DiscoveringLastClue Timing Who Where
  | DiscoverClues Timing Who Where ValueMatcher
  | GainsClues Timing Who ValueMatcher
  | GainsResources Timing Who SourceMatcher ValueMatcher
  | EnemyWouldAttack Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacks Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacksEvenIfCancelled Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacked Timing Who SourceMatcher EnemyMatcher
  | EnemyAttackedSuccessfully Timing Who SourceMatcher EnemyMatcher
  | RevealChaosToken Timing Who ChaosTokenMatcher
  | RevealChaosTokensDuringSkillTest Timing Who SkillTestMatcher ChaosTokenMatcher
  | TokensWouldBeRemovedFromChaosBag Timing ChaosTokenMatcher
  | ResolvesChaosToken Timing Who ChaosTokenMatcher
  | CancelChaosToken Timing Who ChaosTokenMatcher
  | IgnoreChaosToken Timing Who ChaosTokenMatcher
  | WouldRevealChaosToken Timing Who
  | Discarded Timing (Maybe Who) SourceMatcher ExtendedCardMatcher
  | WouldDiscardFromHand Timing Who SourceMatcher
  | AssetHealed Timing DamageType AssetMatcher SourceMatcher
  | InvestigatorHealed Timing DamageType InvestigatorMatcher SourceMatcher
  | AssetWouldBeDiscarded Timing AssetMatcher
  | EventWouldBeDiscarded Timing EventMatcher
  | EnemyWouldBeDiscarded Timing EnemyMatcher
  | TreacheryWouldBeDiscarded Timing TreacheryMatcher
  | EnemyDiscarded Timing SourceMatcher EnemyMatcher
  | TreacheryDiscarded Timing SourceMatcher TreacheryMatcher
  | WouldPerformRevelationSkillTest Timing Who
  | ResolvingRevelation Timing Who TreacheryMatcher
  | InitiatedSkillTest Timing Who SkillTypeMatcher SkillTestValueMatcher SkillTestMatcher
  | SkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SkillTestEnded Timing Who SkillTestMatcher
  | WouldPlaceClueOnLocation Timing Who Where ValueMatcher
  | WouldAddChaosTokensToChaosBag Timing (Maybe Who) ValueMatcher ChaosTokenFace
  | PlacedCounter Timing Who SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnLocation Timing Where SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnEnemy Timing EnemyMatcher SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnAgenda Timing AgendaMatcher SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnAsset Timing AssetMatcher SourceMatcher CounterMatcher ValueMatcher
  | WouldHaveSkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SuccessfullyInvestigatedWithNoClues Timing Who Where
  | EnemyAttemptsToSpawnAt Timing EnemyMatcher LocationMatcher
  | EnemyWouldSpawnAt EnemyMatcher LocationMatcher
  | EnemySpawns Timing Where EnemyMatcher
  | EnemyMovedTo Timing Where MovesVia EnemyMatcher
  | FastPlayerWindow
  | TurnBegins Timing Who
  | TurnWouldEnd Timing Who
  | TurnEnds Timing Who
  | RoundBegins Timing
  | RoundEnds Timing
  | DuringTurn Who
  | Enters Timing Who Where
  | Leaves Timing Who Where
  | WouldMove Timing Who SourceMatcher FromWhere ToWhere
  | Moves Timing Who SourceMatcher FromWhere ToWhere
  | MoveAction Timing Who FromWhere ToWhere
  | OrWindowMatcher [WindowMatcher]
  | DealtDamage Timing SourceMatcher Who
  | DealtHorror Timing SourceMatcher Who
  | AssignedHorror Timing Who TargetListMatcher
  | DealtDamageOrHorror Timing SourceMatcher Who
  | WouldDrawEncounterCard Timing Who PhaseMatcher
  | WouldDrawCard Timing Who DeckMatcher
  | DrawCard Timing Who ExtendedCardMatcher DeckMatcher
  | DrawsCards Timing Who ValueMatcher
  | PlayCard Timing Who ExtendedCardMatcher
  | PlayEventDiscarding Timing Who EventMatcher
  | PhaseBegins Timing PhaseMatcher
  | PhaseEnds Timing PhaseMatcher
  | PlayerHasPlayableCard CostStatus ExtendedCardMatcher
  | RevealLocation Timing Who Where
  | FlipLocation Timing Who Where
  | PutLocationIntoPlay Timing Who Where
  | GameBegins Timing
  | GameEnds Timing
  | InvestigatorEliminated Timing Who
  | InvestigatorResigned Timing Who
  | AnyWindow
  | NotAnyWindow
  | CommittedCards Timing Who CardListMatcher
  | CommittedCard Timing Who CardMatcher
  | ActivateAbility Timing Who AbilityMatcher
  | Explored Timing Who ExploreMatcher
  | AttemptExplore Timing Who
  | PhaseStep Timing PhaseStepMatcher
  | SkillTestStep Timing SkillTestStep
  | AddingToCurrentDepth
  | CancelledOrIgnoredCardOrGameEffect SourceMatcher
  | LostResources Timing Who SourceMatcher
  | LostActions Timing Who SourceMatcher
  | WouldTriggerChaosTokenRevealEffectOnCard Who CardMatcher [ChaosTokenFace]
  | Exhausts Timing Who TargetMatcher
  | EnemyExhausts Timing EnemyMatcher
  | EntersThreatArea Timing Who CardMatcher
  | ScenarioCountIncremented Timing ScenarioCountKey
  | WindowWhen Criterion WindowMatcher
  | ScenarioEvent Timing Text
  | TakeControlOfClues Timing Who SourceMatcher
  deriving stock (Show, Eq, Ord, Data, Generic)

data ExploreMatcher = SuccessfulExplore LocationMatcher | FailedExplore CardMatcher
  deriving stock (Show, Eq, Ord, Data)

data DefeatedByMatcher
  = ByHorror
  | ByDamage
  | ByOther
  | ByAny
  | BySource SourceMatcher
  | ByAnyOf [DefeatedByMatcher]
  | DefeatedByMatches [DefeatedByMatcher]
  | NotBy DefeatedByMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Not DefeatedByMatcher where
  not_ = NotBy

instance Monoid DefeatedByMatcher where
  mempty = ByAny

instance Semigroup DefeatedByMatcher where
  ByAny <> x = x
  x <> ByAny = x
  DefeatedByMatches xs <> DefeatedByMatches ys = DefeatedByMatches (xs <> ys)
  DefeatedByMatches xs <> y = DefeatedByMatches (xs <> [y])
  y <> DefeatedByMatches xs = DefeatedByMatches (y : xs)
  x <> y = DefeatedByMatches [x, y]

mconcat
  [ deriveJSON defaultOptions ''DefeatedByMatcher
  , deriveJSON defaultOptions ''ExploreMatcher
  , deriveToJSON defaultOptions ''WindowMatcher
  ]

instance FromJSON WindowMatcher where
  parseJSON = withObject "WindowMatcher" $ \o -> do
    t :: Text <- o .: "tag"
    case t of
      "EnemyAttackedSuccessfully" -> do
        econtents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case econtents of
          Left (a, b, c) -> pure $ EnemyAttackedSuccessfully a b AnySource c
          Right (a, b, c, d) -> pure $ EnemyAttackedSuccessfully a b c d
      "WouldAddChaosTokensToChaosBag" -> do
        econtents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case econtents of
          Left (a, b, c) -> pure $ WouldAddChaosTokensToChaosBag a Nothing b c
          Right (a, b, c, d) -> pure $ EnemyAttackedSuccessfully a b c d
      _ -> genericParseJSON defaultOptions (Object o)
