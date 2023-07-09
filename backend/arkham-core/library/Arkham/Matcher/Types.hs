{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Types where

import Arkham.Prelude

import Arkham.Act.Sequence (ActSide)
import Arkham.Action (Action)
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Sequence
import Arkham.Asset.Uses
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Cost.Status
import Arkham.Criteria.Override
import Arkham.Damage
import Arkham.Deck
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Key
import Arkham.Keyword (Keyword)
import Arkham.Label
import Arkham.LocationSymbol
import Arkham.Modifier
import Arkham.Phase
import {-# SOURCE #-} Arkham.Placement
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Timing
import Arkham.Token
import Arkham.Trait
import Arkham.Zone
import Data.Aeson.TH

type Who = InvestigatorMatcher

data InvestigatorMatcher
  = InvestigatorAt LocationMatcher
  | InvestigatorIs CardCode
  | You
  | UnengagedInvestigator
  | NoOne
  | NotYou
  | Anyone
  | FewestCardsInHand
  | MostCardsInHand
  | LowestRemainingHealth
  | LowestRemainingSanity
  | MostRemainingSanity
  | MostHorror
  | NearestToEnemy EnemyMatcher
  | NearestToLocation LocationMatcher
  | HasMostMatchingAsset AssetMatcher
  | HasMatchingAsset AssetMatcher
  | HasMatchingEvent EventMatcher
  | HasMatchingSkill SkillMatcher
  | HasMatchingTreachery TreacheryMatcher
  | MostClues
  | MostKeys
  | UneliminatedInvestigator
  | ResignedInvestigator
  | DefeatedInvestigator
  | ContributedMatchingIcons ValueMatcher
  | DeckWith CardListMatcher
  | HandWith CardListMatcher
  | DiscardWith CardListMatcher
  | InvestigatorWithTrait Trait
  | InvestigatorWithoutModifier ModifierType
  | InvestigatorEngagedWith EnemyMatcher
  | InvestigatorWithActionsRemaining ValueMatcher
  | InvestigatorWithClues ValueMatcher
  | InvestigatorWithDamage ValueMatcher
  | InvestigatorWithDoom ValueMatcher
  | InvestigatorWithHorror ValueMatcher
  | InvestigatorWithRemainingSanity ValueMatcher
  | InvestigatorWithResources ValueMatcher
  | InvestigatorWithSpendableResources ValueMatcher
  | InvestigatorWithId InvestigatorId
  | InvestigatorWithTreacheryInHand TreacheryMatcher
  | InvestigatorWithTitle Text
  | InvestigatorMatches [InvestigatorMatcher]
  | InvestigatorWithLowestSkill SkillType
  | InvestigatorWithHighestSkill SkillType
  | AnyInvestigator [InvestigatorMatcher]
  | TurnInvestigator
  | ActiveInvestigator
  | LeadInvestigator
  | NoDamageDealtThisTurn
  | NoSuccessfulExploreThisTurn
  | TopCardOfDeckIs CardMatcher
  | YetToTakeTurn
  | NotInvestigator InvestigatorMatcher
  | InvestigatorThatMovedDuringTurn
  | InvestigatorWithSupply Supply
  | InvestigatorCanDiscoverCluesAtOneOf LocationMatcher -- NOTE: Use matcher above
  | DeckIsEmpty
  | AliveInvestigator
  | IncludeEliminated InvestigatorMatcher
  | HealableInvestigator Source DamageType InvestigatorMatcher
  | InvestigatorWithMostCardsInPlayArea
  | InvestigatorWithClass ClassSymbol
  | InvestigatorWithKey ArkhamKey
  deriving stock (Show, Eq, Ord)

instance Semigroup InvestigatorMatcher where
  InvestigatorMatches xs <> InvestigatorMatches ys =
    InvestigatorMatches $ xs <> ys
  InvestigatorMatches xs <> x = InvestigatorMatches (x : xs)
  x <> InvestigatorMatches xs = InvestigatorMatches (x : xs)
  x <> y = InvestigatorMatches [x, y]

data PreyMatcher
  = Prey InvestigatorMatcher
  | OnlyPrey InvestigatorMatcher
  | BearerOf EnemyId
  deriving stock (Show, Eq, Ord)

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  | AssetWithClass ClassSymbol
  | AssetWithTrait Trait
  | AssetAttachedToAsset AssetMatcher
  | AssetWithAttachedEvent EventMatcher
  | AssetControlledBy InvestigatorMatcher
  | AssetMatches [AssetMatcher]
  | AssetOneOf [AssetMatcher]
  | AssetAtLocation LocationId
  | AssetNonStory
  | AssetReady
  | AssetExhausted
  | AssetCanLeavePlayByNormalMeans
  | AssetWithModifier ModifierType
  | AssetWithoutModifier ModifierType
  | AssetNotAtUseLimit
  | AssetWithUseType UseType
  | AssetWithUses UseType
  | AssetWithUseCount UseType Int
  | AssetWithDoom ValueMatcher
  | AssetWithClues ValueMatcher
  | AssetInSlot SlotType
  | AssetInTwoHandSlots
  | AssetIs CardCode
  | AssetWithCardId CardId
  | AssetCardMatch CardMatcher
  | AnyAsset
  | NotAsset AssetMatcher
  | EnemyAsset EnemyId
  | AssetAt LocationMatcher
  | DiscardableAsset
  | AssetWithDamage
  | AssetWithHorror
  | AssetWithHealth
  | AssetWithFewestClues AssetMatcher
  | AssetCanBeAssignedDamageBy InvestigatorId
  | AssetCanBeAssignedHorrorBy InvestigatorId
  | AssetWithCardsUnderneath CardListMatcher
  | ClosestAsset LocationId AssetMatcher
  | NonWeaknessAsset
  | AssetWithMatchingSkillTestIcon
  | UniqueAsset
  | AssetWithDifferentTitleFromAtLeastOneCardInHand InvestigatorMatcher ExtendedCardMatcher AssetMatcher
  | HealableAsset Source DamageType AssetMatcher
  | AssetWithPlacement Placement
  | AssetWithPerformableAbility AbilityMatcher [ModifierType]
  deriving stock (Show, Eq, Ord)

instance Semigroup AssetMatcher where
  AnyAsset <> x = x
  x <> AnyAsset = x
  AssetMatches xs <> AssetMatches ys = AssetMatches (xs <> ys)
  AssetMatches xs <> x = AssetMatches (x : xs)
  x <> AssetMatches xs = AssetMatches (x : xs)
  x <> y = AssetMatches [x, y]

instance Monoid AssetMatcher where
  mempty = AnyAsset

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | EnemyWithTrait Trait
  | EnemyAt LocationMatcher
  | EnemyWithoutTrait Trait
  | EnemyWithKeyword Keyword
  | EnemyWithClues ValueMatcher
  | EnemyWithDamage ValueMatcher
  | EnemyWithDoom ValueMatcher
  | EnemyIsEngagedWith InvestigatorMatcher
  | EnemyWithAsset AssetMatcher
  | NearestEnemy EnemyMatcher
  | FarthestEnemyFrom InvestigatorId EnemyMatcher
  | FarthestEnemyFromAll EnemyMatcher
  | NearestEnemyTo InvestigatorId EnemyMatcher
  | NearestEnemyToLocation LocationId EnemyMatcher
  | EnemyIs CardCode
  | EnemyWithCardId CardId
  | AnyEnemy
  | CanFightEnemy Source
  | CanEvadeEnemy Source
  | CanFightEnemyWithOverride CriteriaOverride
  | CanEvadeEnemyWithOverride CriteriaOverride
  | CanEngageEnemy
  | ReadyEnemy
  | ExhaustedEnemy
  | NonWeaknessEnemy
  | CanParleyEnemy InvestigatorId
  | EnemyMatchAll [EnemyMatcher]
  | EnemyOneOf [EnemyMatcher]
  | EnemyEngagedWithYou
  | EnemyNotEngagedWithYou
  | EnemyWithMostRemainingHealth EnemyMatcher
  | EnemyWithoutModifier ModifierType
  | EnemyWithModifier ModifierType
  | EnemyWithEvade
  | UnengagedEnemy
  | UniqueEnemy
  | NotEnemy EnemyMatcher
  | MovingEnemy
  | EvadingEnemy
  | IsIchtacasPrey
  | EnemyCanBeDamagedBySource Source
  | OutOfPlayEnemy OutOfPlayZone EnemyMatcher
  | EnemyWithPlacement Placement
  deriving stock (Show, Eq, Ord)

instance Semigroup EnemyMatcher where
  AnyEnemy <> x = x
  x <> AnyEnemy = x
  EnemyMatchAll xs <> EnemyMatchAll ys = EnemyMatchAll (xs <> ys)
  EnemyMatchAll xs <> x = EnemyMatchAll (x : xs)
  x <> EnemyMatchAll xs = EnemyMatchAll (x : xs)
  x <> y = EnemyMatchAll [x, y]

instance Monoid EnemyMatcher where
  mempty = AnyEnemy

data EventMatcher
  = EventWithTitle Text
  | EventWithFullTitle Text Text
  | EventWithId EventId
  | EventWithTrait Trait
  | EventWithClass ClassSymbol
  | EventWithCardId CardId
  | EventControlledBy InvestigatorMatcher
  | EventAt LocationMatcher
  | EventWithDoom ValueMatcher
  | EventAttachedToAsset AssetMatcher
  | EventIs CardCode
  | EventReady
  | EventCardMatch CardMatcher
  | EventMatches [EventMatcher]
  | AnyEvent
  | NotEvent EventMatcher
  | EventWithPlacement Placement
  deriving stock (Show, Eq, Ord)

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

type Where = LocationMatcher

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithUnrevealedTitle Text
  | LocationWithId LocationId
  | LocationWithLabel Label
  | LocationWithSymbol LocationSymbol
  | LocationLeavingPlay
  | LocationWithoutClues
  | LocationWithDoom ValueMatcher
  | YourLocation
  | SameLocation
  | NotYourLocation
  | LocationIs CardCode
  | LocationWithCardId CardId
  | Anywhere
  | Nowhere
  | HauntedLocation
  | EmptyLocation
  | AccessibleLocation
  | ConnectedFrom LocationMatcher
  | ConnectedTo LocationMatcher
  | AccessibleFrom LocationMatcher
  | AccessibleTo LocationMatcher
  | ConnectedLocation
  | LocationWithDistanceFrom Int LocationMatcher
  | LocationWithResources ValueMatcher
  | LocationWithClues ValueMatcher
  | LocationWithHorror ValueMatcher
  | LocationWithMostClues LocationMatcher
  | LocationWithEnemy EnemyMatcher
  | LocationWithAsset AssetMatcher
  | LocationWithInvestigator InvestigatorMatcher
  | RevealedLocation
  | UnrevealedLocation
  | InvestigatableLocation
  | LocationNotInPlay
  | FarthestLocationFromLocation LocationId LocationMatcher
  | NearestLocationToLocation LocationId LocationMatcher
  | --                           ^ start
    FarthestLocationFromYou LocationMatcher
  | FarthestLocationFromAll LocationMatcher
  | NearestLocationToYou LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTrait Trait
  | LocationInDirection Direction LocationMatcher
  | LocationWithTreachery TreacheryMatcher
  | LocationWithoutTreachery TreacheryMatcher
  | LocationWithoutModifier ModifierType
  | LocationWithModifier ModifierType
  | LocationWithDiscoverableCluesBy InvestigatorMatcher
  | LocationMatchAll [LocationMatcher]
  | LocationMatchAny [LocationMatcher]
  | FirstLocation [LocationMatcher]
  | NotLocation LocationMatcher
  | LocationCanBeFlipped
  | SingleSidedLocation
  | ClosestPathLocation LocationId LocationId
  | LocationWithDefeatedEnemyThisRound
  | HighestShroud LocationMatcher
  | -- | start destination / end destination
    LocationWithLowerShroudThan LocationMatcher
  | BlockedLocation
  | -- | only useful for windows
    ThisLocation
  | IsIchtacasDestination
  | -- | Scenario specific criteria
    LocationIsInFrontOf InvestigatorMatcher
  deriving stock (Show, Eq, Ord)

class IsLocationMatcher a where
  toLocationMatcher :: a -> LocationMatcher

instance IsLocationMatcher LocationMatcher where
  toLocationMatcher = id

instance IsLocationMatcher LocationId where
  toLocationMatcher = LocationWithId

instance Semigroup LocationMatcher where
  Nowhere <> _ = Nowhere
  _ <> Nowhere = Nowhere
  LocationMatchAll xs <> LocationMatchAll ys = LocationMatchAll $ xs <> ys
  LocationMatchAll xs <> x = LocationMatchAll (x : xs)
  x <> LocationMatchAll xs = LocationMatchAll (x : xs)
  x <> y = LocationMatchAll [x, y]

newtype AnyLocationMatcher = AnyLocationMatcher {getAnyLocationMatcher :: LocationMatcher}

instance Semigroup AnyLocationMatcher where
  AnyLocationMatcher l <> AnyLocationMatcher r =
    AnyLocationMatcher $ case (l, r) of
      (Nowhere, x) -> x
      (x, Nowhere) -> x
      (LocationMatchAny xs, LocationMatchAny ys) -> LocationMatchAny $ xs <> ys
      (LocationMatchAny xs, x) -> LocationMatchAny (x : xs)
      (x, LocationMatchAny xs) -> LocationMatchAny (x : xs)
      (x, y) -> LocationMatchAny [x, y]

instance Monoid AnyLocationMatcher where
  mempty = AnyLocationMatcher Nowhere

data SkillMatcher
  = SkillWithTitle Text
  | SkillWithFullTitle Text Text
  | SkillWithId SkillId
  | SkillWithTrait Trait
  | SkillWithClass ClassSymbol
  | SkillWithCardId CardId
  | SkillControlledBy InvestigatorMatcher
  | SkillWithPlacement Placement
  | SkillMatches [SkillMatcher]
  | YourSkill
  | AnySkill
  deriving stock (Show, Eq, Ord)

instance Semigroup SkillMatcher where
  AnySkill <> x = x
  x <> AnySkill = x
  SkillMatches xs <> SkillMatches ys = SkillMatches (xs <> ys)
  SkillMatches xs <> x = SkillMatches (x : xs)
  x <> SkillMatches xs = SkillMatches (x : xs)
  x <> y = SkillMatches [x, y]

instance Monoid SkillMatcher where
  mempty = AnySkill

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithDoom ValueMatcher
  | TreacheryWithTrait Trait
  | TreacheryInHandOf InvestigatorMatcher
  | TreacheryInThreatAreaOf InvestigatorMatcher
  | TreacheryIs CardCode
  | TreacheryAt LocationMatcher
  | TreacheryOnEnemy EnemyMatcher
  | TreacheryIsNonWeakness
  | AnyTreachery
  | TreacheryOwnedBy InvestigatorMatcher
  | TreacheryMatches [TreacheryMatcher]
  | TreacheryOneOf [TreacheryMatcher]
  | NotTreachery TreacheryMatcher
  deriving stock (Show, Eq, Ord)

instance Semigroup TreacheryMatcher where
  AnyTreachery <> x = x
  x <> AnyTreachery = x
  TreacheryMatches xs <> TreacheryMatches ys = TreacheryMatches (xs <> ys)
  TreacheryMatches xs <> x = TreacheryMatches (x : xs)
  x <> TreacheryMatches xs = TreacheryMatches (x : xs)
  x <> y = TreacheryMatches [x, y]

instance Monoid TreacheryMatcher where
  mempty = AnyTreachery

-- | Relies on game state, can not be used purely
data ExtendedCardMatcher
  = BasicCardMatch CardMatcher
  | CardIsBeneathInvestigator Who
  | InHandOf Who
  | InDeckOf Who
  | InDiscardOf Who
  | TopOfDeckOf Who
  | EligibleForCurrentSkillTest
  | SetAsideCardMatch CardMatcher
  | UnderScenarioReferenceMatch CardMatcher
  | VictoryDisplayCardMatch CardMatcher
  | HandCardWithDifferentTitleFromAtLeastOneAsset InvestigatorMatcher AssetMatcher CardMatcher
  | ExtendedCardWithOneOf [ExtendedCardMatcher]
  | ExtendedCardMatches [ExtendedCardMatcher]
  | PlayableCardWithCostReduction Int ExtendedCardMatcher
  | PlayableCard CostStatus ExtendedCardMatcher
  | CommittableCard InvestigatorId ExtendedCardMatcher
  | CardWithPerformableAbility AbilityMatcher [ModifierType]
  | CanCancelRevelationEffect ExtendedCardMatcher
  | CanCancelAllEffects ExtendedCardMatcher
  deriving stock (Show, Eq, Ord)

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  ExtendedCardMatches xs <> x = ExtendedCardMatches (x : xs)
  x <> ExtendedCardMatches xs = ExtendedCardMatches (x : xs)
  x <> y = ExtendedCardMatches [x, y]

-- | Only relies on card state, can be used purely with `cardMatch`
data CardMatcher
  = CardWithType CardType
  | CardWithCardCode CardCode
  | CardWithTitle Text
  | CardWithTrait Trait
  | CardWithId CardId
  | CardWithLevel Int
  | CardWithoutKeyword Keyword
  | CardWithClass ClassSymbol
  | CardWithSkillIcon SkillIcon
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | CardWithPrintedLocationConnection LocationSymbol
  | CardWithPrintedLocationSymbol LocationSymbol
  | NotCard CardMatcher
  | IsEncounterCard
  | CardIsUnique
  | FastCard
  | NonWeakness
  | NonSignature
  | WeaknessCard
  | NonExceptional
  | AnyCard
  | CardWithVengeance
  | CardFillsSlot SlotType
  | DiscardableCard
  | CardOwnedBy InvestigatorId
  deriving stock (Show, Eq, Ord)

instance Semigroup CardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatches xs <> CardMatches ys = CardMatches $ xs <> ys
  CardMatches xs <> x = CardMatches (x : xs)
  x <> CardMatches xs = CardMatches (x : xs)
  x <> y = CardMatches [x, y]

instance Monoid CardMatcher where
  mempty = AnyCard

class IsCardMatcher a where
  toCardMatcher :: a -> CardMatcher

instance IsCardMatcher CardMatcher where
  toCardMatcher = id
  {-# INLINE toCardMatcher #-}

data DiscardedPlayerCardMatcher
  = DiscardedCardMatcher
      InvestigatorMatcher
      CardMatcher
  deriving stock (Show, Eq, Ord)

data WindowMatcher
  = EnemyDefeated Timing Who DefeatedByMatcher EnemyMatcher
  | WouldBeShuffledIntoDeck DeckMatcher CardMatcher
  | AddedToVictory Timing CardMatcher
  | PerformAction Timing Who ActionMatcher
  | DrawingStartingHand Timing Who
  | InvestigatorDefeated Timing DefeatedByMatcher Who
  | InvestigatorWouldBeDefeated Timing DefeatedByMatcher Who
  | InvestigatorWouldTakeDamage Timing Who SourceMatcher
  | InvestigatorWouldTakeHorror Timing Who SourceMatcher
  | AmongSearchedCards Who
  | DeckHasNoCards Timing Who
  | EncounterDeckRunsOutOfCards
  | MovedBy Timing Who SourceMatcher
  | MovedButBeforeEnemyEngagement Timing Who Where
  | MovedFromHunter Timing EnemyMatcher
  | ChosenRandomLocation Timing LocationMatcher
  | PlaceUnderneath Timing TargetMatcher CardMatcher
  | PlacedDoomCounter Timing SourceMatcher TargetMatcher
  | EnemyWouldBeDefeated Timing EnemyMatcher
  | EnemyWouldReady Timing EnemyMatcher
  | EnemyEnters Timing Where EnemyMatcher
  | EnemyLeaves Timing Where EnemyMatcher
  | AgendaAdvances Timing AgendaMatcher
  | AgendaWouldAdvance Timing AgendaAdvancementReason AgendaMatcher
  | AssetDefeated Timing DefeatedByMatcher AssetMatcher
  | EnemyEvaded Timing Who EnemyMatcher
  | EnemyEngaged Timing Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | AssetEntersPlay Timing AssetMatcher
  | AssetLeavesPlay Timing AssetMatcher
  | AssetDealtDamage Timing SourceMatcher AssetMatcher
  | LastClueRemovedFromAsset Timing AssetMatcher
  | EnemyDealtDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyDealtExcessDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyTakeDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | InvestigatorTakeDamage Timing Who SourceMatcher
  | InvestigatorTakeHorror Timing Who SourceMatcher
  | EnemyLeavesPlay Timing EnemyMatcher
  | LocationLeavesPlay Timing LocationMatcher
  | TookControlOfAsset Timing Who AssetMatcher
  | DiscoveringLastClue Timing Who Where
  | DiscoverClues Timing Who Where ValueMatcher
  | GainsClues Timing Who ValueMatcher
  | EnemyWouldAttack Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacks Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacksEvenIfCancelled Timing Who EnemyAttackMatcher EnemyMatcher
  | EnemyAttacked Timing Who SourceMatcher EnemyMatcher
  | RevealChaosToken Timing Who TokenMatcher
  | CancelChaosToken Timing Who TokenMatcher
  | IgnoreChaosToken Timing Who TokenMatcher
  | WouldRevealChaosToken Timing Who
  | Discarded Timing Who SourceMatcher CardMatcher
  | AssetHealed Timing DamageType AssetMatcher SourceMatcher
  | InvestigatorHealed Timing DamageType InvestigatorMatcher SourceMatcher
  | AssetWouldBeDiscarded Timing AssetMatcher
  | EnemyWouldBeDiscarded Timing EnemyMatcher
  | WouldPerformRevelationSkillTest Timing Who
  | InitiatedSkillTest Timing Who SkillTypeMatcher SkillTestValueMatcher
  | SkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SkillTestEnded Timing Who SkillTestMatcher
  | PlacedCounter Timing Who SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnLocation Timing Where SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnEnemy Timing EnemyMatcher SourceMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnAgenda Timing AgendaMatcher SourceMatcher CounterMatcher ValueMatcher
  | WouldHaveSkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | EnemyAttemptsToSpawnAt Timing EnemyMatcher LocationMatcher
  | EnemyWouldSpawnAt EnemyMatcher LocationMatcher
  | EnemySpawns Timing Where EnemyMatcher
  | FastPlayerWindow
  | TurnBegins Timing Who
  | TurnEnds Timing Who
  | RoundEnds Timing
  | DuringTurn Who
  | Enters Timing Who Where
  | Leaves Timing Who Where
  | Moves Timing Who SourceMatcher Where Where
  | --                 ^ from ^ to
    MoveAction Timing Who Where Where
  | --                      ^ from ^ to
    OrWindowMatcher [WindowMatcher]
  | DealtDamage Timing SourceMatcher Who
  | DealtHorror Timing SourceMatcher Who
  | AssignedHorror Timing Who TargetListMatcher
  | DealtDamageOrHorror Timing SourceMatcher Who
  | WouldDrawEncounterCard Timing Who PhaseMatcher
  | DrawCard Timing Who ExtendedCardMatcher DeckMatcher
  | DrawsCards Timing Who ValueMatcher
  | PlayCard Timing Who ExtendedCardMatcher
  | PhaseBegins Timing PhaseMatcher
  | PhaseEnds Timing PhaseMatcher
  | PlayerHasPlayableCard ExtendedCardMatcher
  | RevealLocation Timing Who Where
  | FlipLocation Timing Who Where
  | PutLocationIntoPlay Timing Who Where
  | GameBegins Timing
  | GameEnds Timing
  | InvestigatorEliminated Timing Who
  | AnyWindow
  | CommittedCards Timing Who CardListMatcher
  | CommittedCard Timing Who CardMatcher
  | ActivateAbility Timing Who AbilityMatcher
  | Explored Timing Who ExploreMatcher
  | AttemptExplore Timing Who
  | PhaseStep Timing PhaseStepMatcher
  | AddingToCurrentDepth
  | CancelledOrIgnoredCardOrGameEffect SourceMatcher
  | LostResources Timing Who SourceMatcher
  | LostActions Timing Who SourceMatcher
  | WouldTriggerTokenRevealEffectOnCard Who CardMatcher [TokenFace]
  deriving stock (Show, Eq, Ord)

data PhaseStepMatcher = EnemiesAttackStep | HuntersMoveStep
  deriving stock (Show, Eq, Ord)

data ExploreMatcher = SuccessfulExplore LocationMatcher | FailedExplore CardMatcher
  deriving stock (Show, Eq, Ord)

data DefeatedByMatcher
  = ByHorror
  | ByDamage
  | ByOther
  | ByAny
  | BySource SourceMatcher
  | ByAnyOf [DefeatedByMatcher]
  | DefeatedByMatches [DefeatedByMatcher]
  deriving stock (Show, Eq, Ord)

instance Monoid DefeatedByMatcher where
  mempty = ByAny

instance Semigroup DefeatedByMatcher where
  ByAny <> x = x
  x <> ByAny = x
  DefeatedByMatches xs <> DefeatedByMatches ys = DefeatedByMatches (xs <> ys)
  DefeatedByMatches xs <> y = DefeatedByMatches (xs <> [y])
  y <> DefeatedByMatches xs = DefeatedByMatches (y : xs)
  x <> y = DefeatedByMatches [x, y]

data SkillTestMatcher
  = WhileInvestigating LocationMatcher
  | WhileAttackingAnEnemy EnemyMatcher
  | WhileEvadingAnEnemy EnemyMatcher
  | SkillTestWithSkill SkillMatcher
  | SkillTestWithSkillType SkillType
  | AnySkillTest
  | SkillTestWasFailed
  | YourSkillTest SkillTestMatcher
  | SkillTestAtYourLocation
  | SkillTestOnTreachery TreacheryMatcher
  | UsingThis
  | SkillTestSourceMatches SourceMatcher
  | SkillTestMatches [SkillTestMatcher]
  | NotSkillTest SkillTestMatcher
  | SkillTestFromRevelation
  deriving stock (Show, Eq, Ord)

data SourceMatcher
  = SourceWithTrait Trait
  | SourceIsEnemyAttack EnemyMatcher
  | EncounterCardSource
  | SourceMatchesAny [SourceMatcher]
  | SourceOwnedBy InvestigatorMatcher
  | SourceIsType CardType
  | AnySource
  | SourceIsCancelable SourceMatcher
  | SourceMatches [SourceMatcher]
  | NotSource SourceMatcher
  | SourceIs Source
  deriving stock (Show, Eq, Ord)

instance Semigroup SourceMatcher where
  AnySource <> x = x
  x <> AnySource = x
  SourceMatches xs <> SourceMatches ys = SourceMatches $ xs <> ys
  SourceMatches xs <> x = SourceMatches $ xs <> [x]
  x <> SourceMatches xs = SourceMatches $ x : xs
  x <> y = SourceMatches [x, y]

data TargetMatcher
  = TargetIs Target
  | TargetMatchesAny [TargetMatcher]
  | AnyTarget
  | TargetMatches [TargetMatcher]
  | ScenarioCardTarget
  deriving stock (Show, Eq, Ord)

instance Semigroup TargetMatcher where
  AnyTarget <> x = x
  x <> AnyTarget = x
  TargetMatches xs <> TargetMatches ys = TargetMatches $ xs <> ys
  TargetMatches xs <> x = TargetMatches $ xs <> [x]
  x <> TargetMatches xs = TargetMatches $ x : xs
  x <> y = TargetMatches [x, y]

data TargetListMatcher
  = HasTarget TargetMatcher
  | ExcludesTarget TargetMatcher
  | AnyTargetList
  deriving stock (Show, Eq, Ord)

instance Semigroup SkillTestMatcher where
  AnySkillTest <> x = x
  x <> AnySkillTest = x
  SkillTestMatches xs <> SkillTestMatches ys = SkillTestMatches $ xs <> ys
  SkillTestMatches xs <> x = SkillTestMatches $ xs <> [x]
  x <> SkillTestMatches xs = SkillTestMatches $ x : xs
  x <> y = SkillTestMatches [x, y]

data SkillTestResultMatcher
  = FailureResult ValueMatcher
  | SuccessResult ValueMatcher
  | AnyResult
  | ResultOneOf [SkillTestResultMatcher]
  deriving stock (Show, Eq, Ord)

data ValueMatcher
  = LessThan GameValue
  | GreaterThan GameValue
  | LessThanOrEqualTo GameValue
  | GreaterThanOrEqualTo GameValue
  | EqualTo GameValue
  | AnyValue
  deriving stock (Show, Eq, Ord)

data SkillTestValueMatcher
  = SkillTestGameValue ValueMatcher
  | GreaterThanBaseValue
  | AnySkillTestValue
  deriving stock (Show, Eq, Ord)

data TokenMatcher
  = WithNegativeModifier
  | TokenFaceIs TokenFace
  | TokenFaceIsNot TokenFace
  | TokenMatchesAny [TokenMatcher]
  | AnyToken
  | TokenMatches [TokenMatcher]
  | IncludeSealed TokenMatcher
  | WouldReduceYourSkillValueToZero
  deriving stock (Show, Eq, Ord)

instance Semigroup TokenMatcher where
  AnyToken <> x = x
  x <> AnyToken = x
  TokenMatches xs <> TokenMatches ys = TokenMatches $ xs <> ys
  TokenMatches xs <> x = TokenMatches $ xs <> [x]
  x <> TokenMatches xs = TokenMatches $ x : xs
  x <> y = TokenMatches [x, y]

instance Monoid TokenMatcher where
  mempty = AnyToken

data PhaseMatcher = AnyPhase | PhaseIs Phase
  deriving stock (Show, Eq, Ord)

data WindowMythosStepMatcher
  = WhenAllDrawEncounterCard
  | AfterCheckDoomThreshold
  deriving stock (Show, Eq, Ord)

data CounterMatcher = HorrorCounter | DamageCounter | ClueCounter | DoomCounter | ResourceCounter
  deriving stock (Show, Eq, Ord)

data ActionMatcher = ActionIs Action | AnyAction | ActionOneOf [ActionMatcher]
  deriving stock (Show, Eq, Ord)

data AbilityMatcher
  = AbilityOnLocation LocationMatcher
  | AbilityOnStory StoryMatcher
  | AbilityWindow WindowMatcher
  | AbilityIsAction Action
  | AbilityIsActionAbility
  | AbilityIsReactionAbility
  | AbilityIsFastAbility
  | AbilityIsForcedAbility
  | AbilityMatches [AbilityMatcher]
  | AbilityOneOf [AbilityMatcher]
  | AbilityIs Source Int
  | AnyAbility
  | AbilityOnEncounterCard
  | AbilityOnCardControlledBy InvestigatorId
  | AssetAbility AssetMatcher
  | HauntedAbility
  deriving stock (Show, Eq, Ord)

instance Semigroup AbilityMatcher where
  AnyAbility <> x = x
  x <> AnyAbility = x
  AbilityMatches xs <> AbilityMatches ys = AbilityMatches $ xs <> ys
  AbilityMatches xs <> x = AbilityMatches $ xs <> [x]
  x <> AbilityMatches xs = AbilityMatches $ x : xs
  x <> y = AbilityMatches [x, y]

instance Monoid AbilityMatcher where
  mempty = AnyAbility

data CardListMatcher
  = LengthIs ValueMatcher
  | HasCard CardMatcher
  | AnyCards
  | DifferentLengthIsAtLeast Int CardMatcher
  deriving stock (Show, Eq, Ord)

data DeckMatcher
  = EncounterDeck
  | DeckOf InvestigatorMatcher
  | AnyDeck
  | DeckIs DeckSignifier
  deriving stock (Show, Eq, Ord)

data AgendaMatcher
  = AgendaWithId AgendaId
  | AgendaWithDoom ValueMatcher
  | AnyAgenda
  | AgendaWithTreachery TreacheryMatcher
  | AgendaWithSequence AgendaSequence
  | AgendaWithSide AgendaSide
  | AgendaWithDeckId Int
  | NotAgenda AgendaMatcher
  | AgendaMatches [AgendaMatcher]
  deriving stock (Show, Eq, Ord)

instance Semigroup AgendaMatcher where
  AnyAgenda <> x = x
  x <> AnyAgenda = x
  AgendaMatches xs <> AgendaMatches ys = AgendaMatches (xs <> ys)
  AgendaMatches xs <> x = AgendaMatches (x : xs)
  x <> AgendaMatches xs = AgendaMatches (x : xs)
  x <> y = AgendaMatches [x, y]

data ActMatcher
  = ActWithId ActId
  | AnyAct
  | ActWithSide ActSide
  | ActWithTreachery TreacheryMatcher
  | ActWithDeckId Int
  | NotAct ActMatcher
  | ActOneOf [ActMatcher]
  deriving stock (Show, Eq, Ord)

data DamageEffectMatcher
  = AttackDamageEffect
  | NonAttackDamageEffect
  | AnyDamageEffect
  deriving stock (Show, Eq, Ord)

data EnemyAttackMatcher
  = AnyEnemyAttack
  | AttackOfOpportunityAttack
  | CancelableEnemyAttack EnemyAttackMatcher
  deriving stock (Show, Eq, Ord)

data ScenarioMatcher = TheScenario
  deriving stock (Show, Eq, Ord)

data CampaignMatcher = TheCampaign
  deriving stock (Show, Eq, Ord)

data EffectMatcher = AnyEffect
  deriving stock (Show, Eq, Ord)

data SkillTypeMatcher
  = AnySkillType
  | NotSkillType SkillType
  | IsSkillType SkillType
  deriving stock (Show, Eq, Ord)

data RemoveDoomMatchers = RemoveDoomMatchers
  { removeDoomLocations :: LocationMatcher
  , removeDoomInvestigators :: InvestigatorMatcher
  , removeDoomEnemies :: EnemyMatcher
  , removeDoomAssets :: AssetMatcher
  , removeDoomActs :: ActMatcher
  , removeDoomAgendas :: AgendaMatcher
  , removeDoomTreacheries :: TreacheryMatcher
  , removeDoomEvents :: EventMatcher
  , removeDoomSkills :: SkillMatcher
  }
  deriving stock (Show, Eq, Ord)

newtype RemainingActMatcher = RemainingActMatcher {unRemainingActMatcher :: ActMatcher}
  deriving stock (Show, Eq, Ord)

data StoryMatcher
  = StoryWithTitle Text
  | StoryWithPlacement Placement
  | StoryMatchAll [StoryMatcher]
  deriving stock (Show, Eq, Ord)

instance Semigroup StoryMatcher where
  StoryMatchAll xs <> StoryMatchAll ys = StoryMatchAll (xs <> ys)
  StoryMatchAll xs <> x = StoryMatchAll $ xs <> [x]
  x <> StoryMatchAll xs = StoryMatchAll (x : xs)
  x <> y = StoryMatchAll [x, y]

$( do
    ability <- deriveJSON defaultOptions ''AbilityMatcher
    act <- deriveJSON defaultOptions ''ActMatcher
    action <- deriveJSON defaultOptions ''ActionMatcher
    agenda <- deriveJSON defaultOptions ''AgendaMatcher
    asset <- deriveJSON defaultOptions ''AssetMatcher
    card <- deriveJSON defaultOptions ''CardMatcher
    cardList <- deriveJSON defaultOptions ''CardListMatcher
    counter <- deriveJSON defaultOptions ''CounterMatcher
    damageEffect <- deriveJSON defaultOptions ''DamageEffectMatcher
    deck <- deriveJSON defaultOptions ''DeckMatcher
    defeatedBy <- deriveJSON defaultOptions ''DefeatedByMatcher
    enemy <- deriveJSON defaultOptions ''EnemyMatcher
    enemyAttack <- deriveJSON defaultOptions ''EnemyAttackMatcher
    event <- deriveJSON defaultOptions ''EventMatcher
    explore <- deriveJSON defaultOptions ''ExploreMatcher
    extendedCard <- deriveJSON defaultOptions ''ExtendedCardMatcher
    investigator <- deriveJSON defaultOptions ''InvestigatorMatcher
    location <- deriveJSON defaultOptions ''LocationMatcher
    phase <- deriveJSON defaultOptions ''PhaseMatcher
    phaseStep <- deriveJSON defaultOptions ''PhaseStepMatcher
    prey <- deriveJSON defaultOptions ''PreyMatcher
    removeDoom <- deriveJSON defaultOptions ''RemoveDoomMatchers
    skill <- deriveJSON defaultOptions ''SkillMatcher
    skillTest <- deriveJSON defaultOptions ''SkillTestMatcher
    skillTestResult <- deriveJSON defaultOptions ''SkillTestResultMatcher
    skillTestValue <- deriveJSON defaultOptions ''SkillTestValueMatcher
    skillType <- deriveJSON defaultOptions ''SkillTypeMatcher
    source <- deriveJSON defaultOptions ''SourceMatcher
    story <- deriveJSON defaultOptions ''StoryMatcher
    target <- deriveJSON defaultOptions ''TargetMatcher
    targetList <- deriveJSON defaultOptions ''TargetListMatcher
    token <- deriveJSON defaultOptions ''TokenMatcher
    treachery <- deriveJSON defaultOptions ''TreacheryMatcher
    value <- deriveJSON defaultOptions ''ValueMatcher
    window <- deriveJSON defaultOptions ''WindowMatcher
    windowMythosStep <- deriveJSON defaultOptions ''WindowMythosStepMatcher
    pure $
      concat
        [ ability
        , act
        , action
        , agenda
        , asset
        , card
        , cardList
        , counter
        , damageEffect
        , deck
        , defeatedBy
        , enemy
        , enemyAttack
        , event
        , explore
        , extendedCard
        , investigator
        , location
        , phase
        , phaseStep
        , prey
        , removeDoom
        , skill
        , skillTest
        , skillTestResult
        , skillTestValue
        , skillType
        , source
        , story
        , target
        , targetList
        , token
        , treachery
        , value
        , window
        , windowMythosStep
        ]
 )
