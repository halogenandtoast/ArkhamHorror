module Arkham.Matcher where

import Arkham.Prelude

import Arkham.Action (Action)
import Arkham.Agenda.AdvancementReason
import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Keyword (Keyword)
import Arkham.Keyword qualified as Keyword
import Arkham.LocationSymbol
import Arkham.Modifier
import Arkham.Phase
import Arkham.ScenarioLogKey
import Arkham.SkillType
import {-# SOURCE #-} Arkham.Slot
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Timing
import Arkham.Token
import Arkham.Trait

data Matcher = MatchInvestigator InvestigatorMatcher | MatchLocation LocationMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Who = InvestigatorMatcher

pattern InvestigatorWithoutActionsRemaining :: InvestigatorMatcher
pattern InvestigatorWithoutActionsRemaining <-
  InvestigatorWithActionsRemaining (EqualTo (Static 0)) where
  InvestigatorWithoutActionsRemaining =
    InvestigatorWithActionsRemaining (EqualTo (Static 0))

pattern InvestigatorWithAnyActionsRemaining :: InvestigatorMatcher
pattern InvestigatorWithAnyActionsRemaining <-
  InvestigatorWithActionsRemaining (GreaterThan (Static 0)) where
  InvestigatorWithAnyActionsRemaining =
    InvestigatorWithActionsRemaining (GreaterThan (Static 0))

pattern InvestigatorWithAnyDamage :: InvestigatorMatcher
pattern InvestigatorWithAnyDamage <-
  InvestigatorWithDamage (GreaterThan (Static 0)) where
  InvestigatorWithAnyDamage = InvestigatorWithDamage (GreaterThan (Static 0))

pattern InvestigatorWithAnyHorror :: InvestigatorMatcher
pattern InvestigatorWithAnyHorror <-
  InvestigatorWithHorror (GreaterThan (Static 0)) where
  InvestigatorWithAnyHorror = InvestigatorWithHorror (GreaterThan (Static 0))

pattern InvestigatorWithAnyClues :: InvestigatorMatcher
pattern InvestigatorWithAnyClues <-
  InvestigatorWithClues (GreaterThan (Static 0)) where
  InvestigatorWithAnyClues = InvestigatorWithClues (GreaterThan (Static 0))

pattern InvestigatorCanMove :: InvestigatorMatcher
pattern InvestigatorCanMove <-
  InvestigatorWithoutModifier CannotMove where
  InvestigatorCanMove = InvestigatorWithoutModifier CannotMove

data InvestigatorMatcher
  = InvestigatorAt LocationMatcher
  | You
  | UnengagedInvestigator
  | NotYou
  | Anyone
  | UneliminatedInvestigator
  | ResignedInvestigator
  | ContributedMatchingIcons ValueMatcher
  | HandWith CardListMatcher
  | DiscardWith CardListMatcher
  | InvestigatorWithoutModifier ModifierType
  | InvestigatorEngagedWith EnemyMatcher
  | InvestigatorWithActionsRemaining ValueMatcher
  | InvestigatorWithClues ValueMatcher
  | InvestigatorWithDamage ValueMatcher
  | InvestigatorWithHorror ValueMatcher
  | InvestigatorWithRemainingSanity ValueMatcher
  | InvestigatorWithResources ValueMatcher
  | InvestigatorWithId InvestigatorId
  | InvestigatorWithTitle Text
  | InvestigatorMatches [InvestigatorMatcher]
  | InvestigatorWithLowestSkill SkillType
  | AnyInvestigator [InvestigatorMatcher]
  | TurnInvestigator
  | LeadInvestigator
  | NoDamageDealtThisTurn
  | TopCardOfDeckIs CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

replaceYouMatcher
  :: InvestigatorId -> InvestigatorMatcher -> InvestigatorMatcher
replaceYouMatcher iid You = InvestigatorWithId iid
replaceYouMatcher iid (InvestigatorMatches matchers) =
  InvestigatorMatches $ map (replaceYouMatcher iid) matchers
replaceYouMatcher iid (AnyInvestigator matchers) =
  AnyInvestigator $ map (replaceYouMatcher iid) matchers
replaceYouMatcher _ m = m

instance Semigroup InvestigatorMatcher where
  InvestigatorMatches xs <> InvestigatorMatches ys =
    InvestigatorMatches $ xs <> ys
  InvestigatorMatches xs <> x = InvestigatorMatches (x : xs)
  x <> InvestigatorMatches xs = InvestigatorMatches (x : xs)
  x <> y = InvestigatorMatches [x, y]

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  | AssetWithClass ClassSymbol
  | AssetWithTrait Trait
  | AssetOwnedBy InvestigatorMatcher
  | AssetMatches [AssetMatcher]
  | AssetOneOf [AssetMatcher]
  | AssetAtLocation LocationId
  | AssetNonStory
  | AssetReady
  | AssetExhausted
  | AssetCanLeavePlayByNormalMeans
  | AssetWithModifier ModifierType
  | AssetWithoutModifier ModifierType
  | AssetWithUseType UseType
  | AssetWithUses UseType
  | AssetInSlot SlotType
  | AssetIs CardCode
  | AssetCardMatch CardMatcher
  | AnyAsset
  | EnemyAsset EnemyId
  | AssetAt LocationMatcher
  | DiscardableAsset
  | AssetWithDamage
  | AssetWithHorror
  | AssetWithFewestClues AssetMatcher
  | AssetCanBeAssignedDamageBy InvestigatorId
  | AssetCanBeAssignedHorrorBy InvestigatorId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

assetIs :: HasCardCode a => a -> AssetMatcher
assetIs = AssetIs . toCardCode

instance Semigroup AssetMatcher where
  AnyAsset <> x = x
  x <> AnyAsset = x
  AssetMatches xs <> AssetMatches ys = AssetMatches (xs <> ys)
  AssetMatches xs <> x = AssetMatches (x : xs)
  x <> AssetMatches xs = AssetMatches (x : xs)
  x <> y = AssetMatches [x, y]

instance Monoid AssetMatcher where
  mempty = AnyAsset

enemyIs :: HasCardCode a => a -> EnemyMatcher
enemyIs = EnemyIs . toCardCode

pattern AloofEnemy :: EnemyMatcher
pattern AloofEnemy <- EnemyWithKeyword Keyword.Aloof where
  AloofEnemy = EnemyWithKeyword Keyword.Aloof

pattern HunterEnemy :: EnemyMatcher
pattern HunterEnemy <- EnemyWithKeyword Keyword.Hunter where
  HunterEnemy = EnemyWithKeyword Keyword.Hunter

pattern EliteEnemy :: EnemyMatcher
pattern EliteEnemy <- EnemyWithTrait Elite where
  EliteEnemy = EnemyWithTrait Elite

pattern NonEliteEnemy :: EnemyMatcher
pattern NonEliteEnemy <- EnemyWithoutTrait Elite where
  NonEliteEnemy = EnemyWithoutTrait Elite

pattern EnemyWithAnyClues :: EnemyMatcher
pattern EnemyWithAnyClues <- EnemyWithClues (GreaterThan (Static 0)) where
  EnemyWithAnyClues = EnemyWithClues (GreaterThan (Static 0))

pattern EnemyWithAnyDoom :: EnemyMatcher
pattern EnemyWithAnyDoom <- EnemyWithDoom (GreaterThan (Static 0)) where
  EnemyWithAnyDoom = EnemyWithDoom (GreaterThan (Static 0))

pattern EnemyWithAnyDamage :: EnemyMatcher
pattern EnemyWithAnyDamage <- EnemyWithDamage (GreaterThan (Static 0)) where
  EnemyWithAnyDamage = EnemyWithDamage (GreaterThan (Static 0))

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
  | NearestEnemy EnemyMatcher
  | EnemyIs CardCode
  | AnyEnemy
  | CanFightEnemy
  | CanEvadeEnemy
  | CanEngageEnemy
  | ReadyEnemy
  | ExhaustedEnemy
  | NonWeaknessEnemy
  | EnemyMatchAll [EnemyMatcher]
  | EnemyOneOf [EnemyMatcher]
  | EnemyEngagedWithYou
  | EnemyNotEngagedWithYou
  | EnemyWithMostRemainingHealth EnemyMatcher
  | EnemyWithoutModifier ModifierType
  | UnengagedEnemy
  | UniqueEnemy
  | NotEnemy EnemyMatcher
  | MovingEnemy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  | EventOwnedBy InvestigatorMatcher
  | EventMatches [EventMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

type Where = LocationMatcher

pattern LocationWithAnyClues :: LocationMatcher
pattern LocationWithAnyClues <-
  LocationWithClues (GreaterThan (Static 0)) where
  LocationWithAnyClues = LocationWithClues (GreaterThan (Static 0))

pattern LocationWithAnyHorror :: LocationMatcher
pattern LocationWithAnyHorror <-
  LocationWithHorror (GreaterThan (Static 0)) where
  LocationWithAnyHorror = LocationWithHorror (GreaterThan (Static 0))

pattern LocationWithoutHorror :: LocationMatcher
pattern LocationWithoutHorror <- LocationWithHorror (EqualTo (Static 0)) where
  LocationWithoutHorror = LocationWithHorror (EqualTo (Static 0))

pattern LocationWithoutClues :: LocationMatcher
pattern LocationWithoutClues <- LocationWithClues (EqualTo (Static 0)) where
  LocationWithoutClues = LocationWithClues (EqualTo (Static 0))

pattern LocationWithoutDoom :: LocationMatcher
pattern LocationWithoutDoom <- LocationWithDoom (EqualTo (Static 0)) where
  LocationWithoutDoom = LocationWithDoom (EqualTo (Static 0))

pattern LeadInvestigatorLocation :: LocationMatcher
pattern LeadInvestigatorLocation <- LocationWithInvestigator LeadInvestigator where
  LeadInvestigatorLocation = LocationWithInvestigator LeadInvestigator

locationIs :: HasCardCode a => a -> LocationMatcher
locationIs = LocationIs . toCardCode

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithUnrevealedTitle Text
  | LocationWithId LocationId
  | LocationWithLabel Text
  | LocationWithSymbol LocationSymbol
  | LocationLeavingPlay
  | LocationWithDoom ValueMatcher
  | YourLocation
  | SameLocation
  | NotYourLocation
  | LocationIs CardCode
  | Anywhere
  | Unblocked
  | EmptyLocation
  | AccessibleLocation
  | AccessibleFrom LocationMatcher
  | AccessibleTo LocationMatcher
  | ConnectedLocation
  | LocationWithResources ValueMatcher
  | LocationWithClues ValueMatcher
  | LocationWithHorror ValueMatcher
  | LocationWithMostClues LocationMatcher
  | LocationWithoutInvestigators
  | LocationWithoutEnemies
  | LocationWithEnemy EnemyMatcher
  | LocationWithAsset AssetMatcher
  | LocationWithInvestigator InvestigatorMatcher
  | RevealedLocation
  | UnrevealedLocation
  | InvestigatableLocation
  | LocationNotInPlay
  | FarthestLocationFromYou LocationMatcher
  | FarthestLocationFromAll LocationMatcher
  | NearestLocationToYou LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTrait Trait
  | LocationInDirection Direction LocationMatcher
  | LocationWithTreachery TreacheryMatcher
  | LocationWithoutTreachery TreacheryMatcher
  | LocationMatchAll [LocationMatcher]
  | LocationMatchAny [LocationMatcher]
  | FirstLocation [LocationMatcher]
  -- only useful for windows
  | ThisLocation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup LocationMatcher where
  LocationMatchAll xs <> LocationMatchAll ys = LocationMatchAll $ xs <> ys
  LocationMatchAll xs <> x = LocationMatchAll (x : xs)
  x <> LocationMatchAll xs = LocationMatchAll (x : xs)
  x <> y = LocationMatchAll [x, y]

data SkillMatcher
  = SkillWithTitle Text
  | SkillWithFullTitle Text Text
  | SkillWithId SkillId
  | SkillWithTrait Trait
  | SkillWithClass ClassSymbol
  | SkillOwnedBy InvestigatorId
  | SkillMatches [SkillMatcher]
  | YourSkill
  | AnySkill
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup SkillMatcher where
  AnySkill <> x = x
  x <> AnySkill = x
  SkillMatches xs <> SkillMatches ys = SkillMatches (xs <> ys)
  SkillMatches xs <> x = SkillMatches (x : xs)
  x <> SkillMatches xs = SkillMatches (x : xs)
  x <> y = SkillMatches [x, y]

instance Monoid SkillMatcher where
  mempty = AnySkill

treacheryIs :: HasCardCode a => a -> TreacheryMatcher
treacheryIs = TreacheryIs . toCardCode

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithTrait Trait
  | TreacheryInHandOf InvestigatorMatcher
  | TreacheryInThreatAreaOf InvestigatorMatcher
  | TreacheryIs CardCode
  | TreacheryAt LocationMatcher
  | AnyTreachery
  | TreacheryOwnedBy InvestigatorMatcher
  | TreacheryMatches [TreacheryMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup TreacheryMatcher where
  AnyTreachery <> x = x
  x <> AnyTreachery = x
  TreacheryMatches xs <> TreacheryMatches ys = TreacheryMatches (xs <> ys)
  TreacheryMatches xs <> x = TreacheryMatches (x : xs)
  x <> TreacheryMatches xs = TreacheryMatches (x : xs)
  x <> y = TreacheryMatches [x, y]

instance Monoid TreacheryMatcher where
  mempty = AnyTreachery

pattern NonWeaknessTreachery :: CardMatcher
pattern NonWeaknessTreachery =
  CardMatches [NonWeakness, CardWithType TreacheryType]

pattern NonPeril :: CardMatcher
pattern NonPeril <- CardWithoutKeyword Keyword.Peril where
  NonPeril = CardWithoutKeyword Keyword.Peril

pattern EventCard :: CardMatcher
pattern EventCard <- CardWithType EventType where
  EventCard = CardWithType EventType

pattern AssetCard :: CardMatcher
pattern AssetCard <- CardWithType AssetType where
  AssetCard = CardWithType AssetType

-- | Relies on game state, can not be used purely
data ExtendedCardMatcher
  = BasicCardMatch CardMatcher
  | CardIsBeneathInvestigator Who
  | InHandOf Who
  | InDiscardOf Who
  | TopOfDeckOf Who
  | EligibleForCurrentSkillTest
  | SetAsideCardMatch CardMatcher
  | UnderScenarioReferenceMatch CardMatcher
  | VictoryDisplayCardMatch CardMatcher
  | ExtendedCardWithOneOf [ExtendedCardMatcher]
  | ExtendedCardMatches [ExtendedCardMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup ExtendedCardMatcher where
  ExtendedCardMatches xs <> ExtendedCardMatches ys =
    ExtendedCardMatches $ xs <> ys
  ExtendedCardMatches xs <> x = ExtendedCardMatches (x : xs)
  x <> ExtendedCardMatches xs = ExtendedCardMatches (x : xs)
  x <> y = ExtendedCardMatches [x, y]

cardIs :: HasCardCode a => a -> CardMatcher
cardIs = CardWithCardCode . toCardCode

pattern IsAlly :: CardMatcher
pattern IsAlly <-
  CardMatches [CardWithType AssetType, CardWithTrait Ally] where
  IsAlly = CardMatches [CardWithType AssetType, CardWithTrait Ally]

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
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | NotCard CardMatcher
  | IsEncounterCard
  | CardIsUnique
  | NonWeakness
  | WeaknessCard
  | NonExceptional
  | AnyCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatches xs <> CardMatches ys = CardMatches $ xs <> ys
  CardMatches xs <> x = CardMatches (x : xs)
  x <> CardMatches xs = CardMatches (x : xs)
  x <> y = CardMatches [x, y]

instance Monoid CardMatcher where
  mempty = AnyCard

data WindowMatcher
  = EnemyDefeated Timing Who EnemyMatcher
  | AddedToVictory Timing CardMatcher
  | PerformAction Timing Who ActionMatcher
  | DrawingStartingHand Timing Who
  | InvestigatorDefeated Timing SourceMatcher Who
  | AmongSearchedCards Who
  | DeckHasNoCards Timing Who
  | MovedBy Timing Who SourceMatcher
  | MovedButBeforeEnemyEngagement Timing Who Where
  | MovedFromHunter Timing EnemyMatcher
  | ChosenRandomLocation Timing LocationMatcher
  | PlaceUnderneath Timing TargetMatcher CardMatcher
  | EnemyWouldBeDefeated Timing EnemyMatcher
  | EnemyWouldReady Timing EnemyMatcher
  | EnemyEnters Timing Where EnemyMatcher
  | EnemyLeaves Timing Where EnemyMatcher
  | AgendaAdvances Timing AgendaMatcher
  | AgendaWouldAdvance Timing AgendaAdvancementReason AgendaMatcher
  | AssetDefeated Timing AssetMatcher
  | EnemyEvaded Timing Who EnemyMatcher
  | EnemyEngaged Timing Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | AssetEntersPlay Timing AssetMatcher
  | AssetLeavesPlay Timing AssetMatcher
  | AssetDealtDamage Timing AssetMatcher
  | LastClueRemovedFromAsset Timing AssetMatcher
  | EnemyDealtDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyTakeDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyLeavesPlay Timing EnemyMatcher
  | LocationLeavesPlay Timing LocationMatcher
  | TookControlOfAsset Timing Who AssetMatcher
  | DiscoveringLastClue Timing Who Where
  | DiscoverClues Timing Who Where ValueMatcher
  | GainsClues Timing Who ValueMatcher
  | EnemyWouldAttack Timing Who EnemyMatcher
  | EnemyAttacks Timing Who EnemyMatcher
  | EnemyAttacked Timing Who SourceMatcher EnemyMatcher
  | RevealChaosToken Timing Who TokenMatcher
  | WouldRevealChaosToken Timing Who
  | Discarded Timing Who CardMatcher
  | AssetWouldBeDiscarded Timing AssetMatcher
  | EnemyWouldBeDiscarded Timing EnemyMatcher
  | InitiatedSkillTest Timing Who SkillTestMatcher ValueMatcher
  | SkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SkillTestEnded Timing Who SkillTestMatcher
  | PlacedCounter Timing Who CounterMatcher ValueMatcher
  | PlacedCounterOnLocation Timing Where CounterMatcher ValueMatcher
  | PlacedCounterOnEnemy Timing EnemyMatcher CounterMatcher ValueMatcher
  | WouldHaveSkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | EnemyAttemptsToSpawnAt Timing EnemyMatcher LocationMatcher
  | EnemySpawns Timing Where EnemyMatcher
  | FastPlayerWindow
  | TurnBegins Timing Who
  | TurnEnds Timing Who
  | RoundEnds Timing
  | DuringTurn Who
  | Enters Timing Who Where
  | Leaves Timing Who Where
  | Moves Timing Who Where Where
  --                 ^ from ^ to
  | MoveAction Timing Who Where Where
  --                      ^ from ^ to
  | OrWindowMatcher [WindowMatcher]
  | DealtDamage Timing Who
  | DealtHorror Timing Who
  | AssignedHorror Timing Who TargetListMatcher
  | DealtDamageOrHorror Timing Who
  | WouldDrawEncounterCard Timing Who
  | DrawCard Timing Who ExtendedCardMatcher DeckMatcher
  | PlayCard Timing Who ExtendedCardMatcher
  | PhaseBegins Timing PhaseMatcher
  | PhaseEnds Timing PhaseMatcher
  | PlayerHasPlayableCard ExtendedCardMatcher
  | RevealLocation Timing Who Where
  | PutLocationIntoPlay Timing Who Where
  | GameEnds Timing
  | InvestigatorEliminated Timing Who
  | AnyWindow
  | CommittedCards Timing Who CardListMatcher
  | CommittedCard Timing Who CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SourceMatcher
  = SourceWithTrait Trait
  | SourceIs Source
  | EncounterCardSource
  | SourceMatchesAny [SourceMatcher]
  | SourceOwnedBy InvestigatorMatcher
  | SourceIsType CardType
  | AnySource
  | SourceMatches [SourceMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup SkillTestMatcher where
  AnySkillTest <> x = x
  x <> AnySkillTest = x
  SkillTestMatches xs <> SkillTestMatches ys = SkillTestMatches $ xs <> ys
  SkillTestMatches xs <> x = SkillTestMatches $ xs <> [x]
  x <> SkillTestMatches xs = SkillTestMatches $ x : xs
  x <> y = SkillTestMatches [x, y]

data SkillTestResultMatcher = FailureResult ValueMatcher | SuccessResult ValueMatcher | AnyResult
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AtLeast :: GameValue Int -> ValueMatcher
pattern AtLeast n <- GreaterThanOrEqualTo n where
  AtLeast n = GreaterThanOrEqualTo n

pattern AtMost :: GameValue Int -> ValueMatcher
pattern AtMost n <- LessThanOrEqualTo n where
  AtMost n = LessThanOrEqualTo n

data ValueMatcher
  = LessThan (GameValue Int)
  | GreaterThan (GameValue Int)
  | LessThanOrEqualTo (GameValue Int)
  | GreaterThanOrEqualTo (GameValue Int)
  | EqualTo (GameValue Int)
  | AnyValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data TokenMatcher
  = WithNegativeModifier
  | TokenFaceIs TokenFace
  | TokenFaceIsNot TokenFace
  | TokenMatchesAny [TokenMatcher]
  | AnyToken
  | TokenMatches [TokenMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup TokenMatcher where
  AnyToken <> x = x
  x <> AnyToken = x
  TokenMatches xs <> TokenMatches ys = TokenMatches $ xs <> ys
  TokenMatches xs <> x = TokenMatches $ xs <> [x]
  x <> TokenMatches xs = TokenMatches $ x : xs
  x <> y = TokenMatches [x, y]

data PhaseMatcher = AnyPhase | PhaseIs Phase
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMythosStepMatcher = WhenAllDrawEncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CounterMatcher = HorrorCounter | DamageCounter | ClueCounter
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype ActionMatcher = ActionIs Action
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AbilityMatcher
  = AbilityOnLocation LocationMatcher
  | AbilityWindow WindowMatcher
  | AbilityIsAction Action
  | AbilityIsActionAbility
  | AbilityMatches [AbilityMatcher]
  | AnyAbility
  | AbilityOnScenarioCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup AbilityMatcher where
  AnyAbility <> x = x
  x <> AnyAbility = x
  AbilityMatches xs <> AbilityMatches ys = AbilityMatches $ xs <> ys
  AbilityMatches xs <> x = AbilityMatches $ xs <> [x]
  x <> AbilityMatches xs = AbilityMatches $ x : xs
  x <> y = AbilityMatches [x, y]

instance Monoid AbilityMatcher where
  mempty = AnyAbility

data CardListMatcher = LengthIs ValueMatcher | HasCard CardMatcher | AnyCards
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ScenarioLogKeyListMatcher = RememberedLengthIs ValueMatcher | HasRemembered ScenarioLogKey
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DeckMatcher = EncounterDeck | DeckOf InvestigatorMatcher | AnyDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AgendaMatcher = AgendaWithId AgendaId | AnyAgenda
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ActMatcher = ActWithId ActId | AnyAct
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DamageEffectMatcher = AttackDamageEffect | NonAttackDamageEffect | AnyDamageEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
