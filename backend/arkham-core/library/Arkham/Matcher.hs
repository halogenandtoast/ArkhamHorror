module Arkham.Matcher where

import Arkham.Prelude

import Arkham.Act.Sequence ( ActSide )
import Arkham.Action ( Action )
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Sequence
import Arkham.Asset.Uses
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card.CardCode
import Arkham.Card.CardType
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Criteria.Override
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Keyword ( Keyword )
import Arkham.Keyword qualified as Keyword
import Arkham.Label
import Arkham.LocationSymbol
import Arkham.Modifier
import Arkham.Phase
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Target
import Arkham.Timing
import Arkham.Token
import Arkham.Trait

data Matcher
  = MatchInvestigator InvestigatorMatcher
  | MatchLocation LocationMatcher
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

pattern InvestigatorWithoutAnyClues :: InvestigatorMatcher
pattern InvestigatorWithoutAnyClues <-
  InvestigatorWithClues (EqualTo (Static 0)) where
  InvestigatorWithoutAnyClues = InvestigatorWithClues (EqualTo (Static 0))

pattern InvestigatorWithAnyResources :: InvestigatorMatcher
pattern InvestigatorWithAnyResources <-
  InvestigatorWithResources (GreaterThan (Static 0)) where
  InvestigatorWithAnyResources =
    InvestigatorWithResources (GreaterThan (Static 0))

-- placeholder in case a modifier prevents spending resources
pattern InvestigatorCanSpendResources :: GameValue -> InvestigatorMatcher
pattern InvestigatorCanSpendResources value <-
  InvestigatorWithResources (AtLeast value) where
  InvestigatorCanSpendResources value =
    InvestigatorWithResources (AtLeast value)

pattern InvestigatorCanDisengage :: InvestigatorMatcher
pattern InvestigatorCanDisengage =
  InvestigatorMatches [InvestigatorWithoutModifier CannotDisengageEnemies, InvestigatorEngagedWith AnyEnemy]

pattern InvestigatorCanDiscoverCluesAt
  :: LocationMatcher -> InvestigatorMatcher
pattern InvestigatorCanDiscoverCluesAt locationMatcher =
  InvestigatorMatches [InvestigatorCanDiscoverCluesAtOneOf locationMatcher, InvestigatorWithoutModifier CannotDiscoverClues]

pattern InvestigatorCanMove :: InvestigatorMatcher
pattern InvestigatorCanMove <- InvestigatorWithoutModifier CannotMove where
  InvestigatorCanMove = InvestigatorWithoutModifier CannotMove

pattern InvestigatorCanHealHorror :: InvestigatorMatcher
pattern InvestigatorCanHealHorror <-
  InvestigatorWithoutModifier CannotHealHorror where
  InvestigatorCanHealHorror = InvestigatorWithoutModifier CannotHealHorror

-- Placeholder
pattern InvestigatorCanHealDamage :: InvestigatorMatcher
pattern InvestigatorCanHealDamage <-
  InvestigatorWithoutModifier CannotHealHorror where
  InvestigatorCanHealDamage = Anyone

colocatedWith :: InvestigatorId -> InvestigatorMatcher
colocatedWith = InvestigatorAt . LocationWithInvestigator . InvestigatorWithId

investigatorEngagedWith :: EnemyId -> InvestigatorMatcher
investigatorEngagedWith = InvestigatorEngagedWith . EnemyWithId

investigatorAt :: LocationId -> InvestigatorMatcher
investigatorAt = InvestigatorAt . LocationWithId

data InvestigatorMatcher
  = InvestigatorAt LocationMatcher
  | You
  | UnengagedInvestigator
  | NoOne
  | NotYou
  | Anyone
  | FewestCardsInHand
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
  | UneliminatedInvestigator
  | ResignedInvestigator
  | DefeatedInvestigator
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
  | InvestigatorWithTreacheryInHand TreacheryMatcher
  | InvestigatorWithTitle Text
  | InvestigatorMatches [InvestigatorMatcher]
  | InvestigatorWithLowestSkill SkillType
  | InvestigatorWithHighestSkill SkillType
  | AnyInvestigator [InvestigatorMatcher]
  | TurnInvestigator
  | LeadInvestigator
  | NoDamageDealtThisTurn
  | NoSuccessfulExploreThisTurn
  | TopCardOfDeckIs CardMatcher
  | YetToTakeTurn
  | NotInvestigator InvestigatorMatcher
  | InvestigatorWithSupply Supply
  | InvestigatorCanDiscoverCluesAtOneOf LocationMatcher -- NOTE: Use matcher above
  | DeckIsEmpty
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

data PreyMatcher
  = Prey InvestigatorMatcher
  | OnlyPrey InvestigatorMatcher
  | BearerOf EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

preyWith :: PreyMatcher -> InvestigatorMatcher -> PreyMatcher
preyWith (Prey m1) m2 = Prey $ m1 <> m2
preyWith (OnlyPrey m1) _ = OnlyPrey m1 -- I do not think we should combine here
preyWith (BearerOf m1) _ = BearerOf m1 -- I do not think we should combine here

pattern AllyAsset :: AssetMatcher
pattern AllyAsset <- AssetWithTrait Ally where
  AllyAsset = AssetWithTrait Ally

pattern AssetWithAnyDoom :: AssetMatcher
pattern AssetWithAnyDoom <- AssetWithDoom (GreaterThan (Static 0)) where
  AssetWithAnyDoom = AssetWithDoom (GreaterThan (Static 0))

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithId AssetId
  | AssetWithClass ClassSymbol
  | AssetWithTrait Trait
  | AssetAttachedToAsset AssetMatcher
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
  | AssetWithUseType UseType
  | AssetWithUses UseType
  | AssetWithDoom ValueMatcher
  | AssetWithClues ValueMatcher
  | AssetInSlot SlotType
  | AssetIs CardCode
  | AssetCardMatch CardMatcher
  | AnyAsset
  | NotAsset AssetMatcher
  | EnemyAsset EnemyId
  | AssetAt LocationMatcher
  | DiscardableAsset
  | AssetWithDamage
  | AssetWithHorror
  | AssetWithFewestClues AssetMatcher
  | AssetCanBeAssignedDamageBy InvestigatorId
  | AssetCanBeAssignedHorrorBy InvestigatorId
  | ClosestAsset LocationId AssetMatcher
  | NonWeaknessAsset
  | AssetWithMatchingSkillTestIcon
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

assetIs :: HasCardCode a => a -> AssetMatcher
assetIs = AssetIs . toCardCode

assetControlledBy :: InvestigatorId -> AssetMatcher
assetControlledBy = AssetControlledBy . InvestigatorWithId

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

enemyAt :: LocationId -> EnemyMatcher
enemyAt = EnemyAt . LocationWithId

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

enemyEngagedWith :: InvestigatorId -> EnemyMatcher
enemyEngagedWith = EnemyIsEngagedWith . InvestigatorWithId

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
  | AnyEnemy
  | CanFightEnemy
  | CanFightEnemyWithOverride CriteriaOverride
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
  | EnemyWithModifier ModifierType
  | EnemyWithEvade
  | UnengagedEnemy
  | UniqueEnemy
  | NotEnemy EnemyMatcher
  | MovingEnemy
  | IsIchtacasPrey
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

data VoidEnemyMatcher = AnyVoidEnemy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype SetAsideMatcher matcher = SetAsideMatcher { unSetAsideMatcher :: matcher }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Hashable)

instance Monoid matcher => Monoid (SetAsideMatcher matcher) where
  mempty = SetAsideMatcher mempty

instance Semigroup matcher => Semigroup (SetAsideMatcher matcher) where
  SetAsideMatcher a <> SetAsideMatcher b = SetAsideMatcher (a <> b)

data EventMatcher
  = EventWithTitle Text
  | EventWithFullTitle Text Text
  | EventWithId EventId
  | EventWithTrait Trait
  | EventWithClass ClassSymbol
  | EventControlledBy InvestigatorMatcher
  | EventAt LocationMatcher
  | EventAttachedToAsset AssetMatcher
  | EventReady
  | EventMatches [EventMatcher]
  | AnyEvent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

type Where = LocationMatcher

pattern LocationWithAnyDoom :: LocationMatcher
pattern LocationWithAnyDoom <- LocationWithDoom (GreaterThan (Static 0)) where
  LocationWithAnyDoom = LocationWithDoom (GreaterThan (Static 0))

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

-- pattern LocationWithoutClues :: LocationMatcher
-- pattern LocationWithoutClues <- LocationWithClues (EqualTo (Static 0)) where
--   LocationWithoutClues = LocationWithClues (EqualTo (Static 0))

pattern LocationWithoutDoom :: LocationMatcher
pattern LocationWithoutDoom <- LocationWithDoom (EqualTo (Static 0)) where
  LocationWithoutDoom = LocationWithDoom (EqualTo (Static 0))

pattern LeadInvestigatorLocation :: LocationMatcher
pattern LeadInvestigatorLocation <-
  LocationWithInvestigator LeadInvestigator where
  LeadInvestigatorLocation = LocationWithInvestigator LeadInvestigator

pattern LocationWithoutEnemies :: LocationMatcher
pattern LocationWithoutEnemies <-
  NotLocation (LocationWithEnemy AnyEnemy) where
  LocationWithoutEnemies = NotLocation (LocationWithEnemy AnyEnemy)

pattern LocationWithoutInvestigators :: LocationMatcher
pattern LocationWithoutInvestigators <-
  NotLocation (LocationWithInvestigator Anyone) where
  LocationWithoutInvestigators = NotLocation (LocationWithInvestigator Anyone)

pattern Unblocked :: LocationMatcher
pattern Unblocked <- LocationWithoutModifier Blocked where
  Unblocked = LocationWithoutModifier Blocked

pattern LocationOfThis :: LocationMatcher
pattern LocationOfThis <- SameLocation where
  LocationOfThis = SameLocation

locationIs :: HasCardCode a => a -> LocationMatcher
locationIs = LocationIs . toCardCode
{-# INLINE locationIs #-}

locationWithEnemy :: EnemyId -> LocationMatcher
locationWithEnemy = LocationWithEnemy . EnemyWithId
{-# INLINE locationWithEnemy #-}

locationWithInvestigator :: InvestigatorId -> LocationMatcher
locationWithInvestigator = LocationWithInvestigator . InvestigatorWithId
{-# INLINE locationWithInvestigator #-}

locationWithTreachery :: TreacheryId -> LocationMatcher
locationWithTreachery = LocationWithTreachery . TreacheryWithId
{-# INLINE locationWithTreachery #-}

locationWithoutTreachery :: HasCardCode a => a -> LocationMatcher
locationWithoutTreachery = LocationWithoutTreachery . treacheryIs
{-# INLINE locationWithoutTreachery #-}

accessibleFrom :: LocationId -> LocationMatcher
accessibleFrom = AccessibleFrom . LocationWithId
{-# INLINE accessibleFrom #-}

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
  | Anywhere
  | Nowhere
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
    --                           ^ start
  | FarthestLocationFromYou LocationMatcher
  | FarthestLocationFromAll LocationMatcher
  | NearestLocationToYou LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTrait Trait
  | LocationInDirection Direction LocationMatcher
  | LocationWithTreachery TreacheryMatcher
  | LocationWithoutTreachery TreacheryMatcher
  | LocationWithoutModifier ModifierType
  | LocationWithModifier ModifierType
  | LocationMatchAll [LocationMatcher]
  | LocationMatchAny [LocationMatcher]
  | FirstLocation [LocationMatcher]
  | NotLocation LocationMatcher
  | LocationCanBeFlipped
  | ClosestPathLocation LocationId LocationId
  -- ^ start destination / end destination
  | BlockedLocation
  | ThisLocation
  -- ^ only useful for windows
  | IsIchtacasDestination
  -- ^ Scenario specific criteria
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup LocationMatcher where
  Nowhere <> _ = Nowhere
  _ <> Nowhere = Nowhere
  LocationMatchAll xs <> LocationMatchAll ys = LocationMatchAll $ xs <> ys
  LocationMatchAll xs <> x = LocationMatchAll (x : xs)
  x <> LocationMatchAll xs = LocationMatchAll (x : xs)
  x <> y = LocationMatchAll [x, y]

newtype AnyLocationMatcher = AnyLocationMatcher { getAnyLocationMatcher :: LocationMatcher }

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
  | SkillControlledBy InvestigatorMatcher
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

treacheryInHandOf :: InvestigatorId -> TreacheryMatcher
treacheryInHandOf = TreacheryInHandOf . InvestigatorWithId

treacheryInThreatAreaOf :: InvestigatorId -> TreacheryMatcher
treacheryInThreatAreaOf = TreacheryInThreatAreaOf . InvestigatorWithId

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithTrait Trait
  | TreacheryInHandOf InvestigatorMatcher
  | TreacheryInThreatAreaOf InvestigatorMatcher
  | TreacheryIs CardCode
  | TreacheryAt LocationMatcher
  | TreacheryOnEnemy EnemyMatcher
  | AnyTreachery
  | TreacheryOwnedBy InvestigatorMatcher
  | TreacheryMatches [TreacheryMatcher]
  | TreacheryOneOf [TreacheryMatcher]
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
  | InDeckOf Who
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
  | CardWithSkill SkillType
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | CardWithPrintedLocationConnection LocationSymbol
  | CardWithPrintedLocationSymbol LocationSymbol
  | NotCard CardMatcher
  | IsEncounterCard
  | CardIsUnique
  | FastCard
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

data DiscardedPlayerCardMatcher = DiscardedCardMatcher
  InvestigatorMatcher
  CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMatcher
  = EnemyDefeated Timing Who EnemyMatcher
  | AddedToVictory Timing CardMatcher
  | PerformAction Timing Who ActionMatcher
  | DrawingStartingHand Timing Who
  | InvestigatorDefeated Timing SourceMatcher DefeatedByMatcher Who
  | InvestigatorWouldBeDefeated Timing SourceMatcher DefeatedByMatcher Who
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
  | AssetDefeated Timing DefeatedByMatcher AssetMatcher
  | EnemyEvaded Timing Who EnemyMatcher
  | EnemyEngaged Timing Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | AssetEntersPlay Timing AssetMatcher
  | AssetLeavesPlay Timing AssetMatcher
  | AssetDealtDamage Timing AssetMatcher
  | LastClueRemovedFromAsset Timing AssetMatcher
  | EnemyDealtDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyDealtExcessDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
  | EnemyTakeDamage Timing DamageEffectMatcher EnemyMatcher SourceMatcher
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
  | WouldRevealChaosToken Timing Who
  | Discarded Timing Who CardMatcher
  | AssetWouldBeDiscarded Timing AssetMatcher
  | EnemyWouldBeDiscarded Timing EnemyMatcher
  | InitiatedSkillTest Timing Who SkillTypeMatcher ValueMatcher
  | SkillTestResult Timing Who SkillTestMatcher SkillTestResultMatcher
  | SkillTestEnded Timing Who SkillTestMatcher
  | PlacedCounter Timing Who CounterMatcher ValueMatcher
  | PlacedCounterOnLocation Timing Where CounterMatcher ValueMatcher
  | PlacedCounterOnEnemy Timing EnemyMatcher CounterMatcher ValueMatcher
  | PlacedCounterOnAgenda Timing AgendaMatcher CounterMatcher ValueMatcher
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
  | DealtDamage Timing SourceMatcher Who
  | DealtHorror Timing SourceMatcher Who
  | AssignedHorror Timing Who TargetListMatcher
  | DealtDamageOrHorror Timing SourceMatcher Who
  | WouldDrawEncounterCard Timing Who PhaseMatcher
  | DrawCard Timing Who ExtendedCardMatcher DeckMatcher
  | PlayCard Timing Who ExtendedCardMatcher
  | PhaseBegins Timing PhaseMatcher
  | PhaseEnds Timing PhaseMatcher
  | PlayerHasPlayableCard ExtendedCardMatcher
  | RevealLocation Timing Who Where
  | FlipLocation Timing Who Where
  | PutLocationIntoPlay Timing Who Where
  | GameEnds Timing
  | InvestigatorEliminated Timing Who
  | AnyWindow
  | CommittedCards Timing Who CardListMatcher
  | CommittedCard Timing Who CardMatcher
  | ActivateAbility Timing Who AbilityMatcher
  | Explored Timing Who ExploreMatcher
  | AttemptExplore Timing Who
  | PhaseStep Timing PhaseStepMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data PhaseStepMatcher = EnemiesAttackStep
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ExploreMatcher = SuccessfulExplore LocationMatcher | FailedExplore
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DefeatedByMatcher
  = ByHorror
  | ByDamage
  | ByOther
  | ByAny
  | ByAnyOf [DefeatedByMatcher]
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
  | NotSkillTest SkillTestMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SourceMatcher
  = SourceWithTrait Trait
  | SourceIsEnemyAttack EnemyMatcher
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

data SkillTestResultMatcher
  = FailureResult ValueMatcher
  | SuccessResult ValueMatcher
  | AnyResult
  | ResultOneOf [SkillTestResultMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AtLeast :: GameValue -> ValueMatcher
pattern AtLeast n <- GreaterThanOrEqualTo n where
  AtLeast n = GreaterThanOrEqualTo n

pattern AtMost :: GameValue -> ValueMatcher
pattern AtMost n <- LessThanOrEqualTo n where
  AtMost n = LessThanOrEqualTo n

data ValueMatcher
  = LessThan GameValue
  | GreaterThan GameValue
  | LessThanOrEqualTo GameValue
  | GreaterThanOrEqualTo GameValue
  | EqualTo GameValue
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

data CounterMatcher = HorrorCounter | DamageCounter | ClueCounter | DoomCounter
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ActionMatcher = ActionIs Action | AnyAction | ActionOneOf [ActionMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data AbilityMatcher
  = AbilityOnLocation LocationMatcher
  | AbilityWindow WindowMatcher
  | AbilityIsAction Action
  | AbilityIsActionAbility
  | AbilityIsReactionAbility
  | AbilityMatches [AbilityMatcher]
  | AbilityOneOf [AbilityMatcher]
  | AnyAbility
  | AbilityOnScenarioCard
  | AssetAbility AssetMatcher
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

pattern AgendaWithAnyDoom :: AgendaMatcher
pattern AgendaWithAnyDoom <- AgendaWithDoom (GreaterThan (Static 0)) where
  AgendaWithAnyDoom = AgendaWithDoom (GreaterThan (Static 0))

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype RemainingActMatcher = RemainingActMatcher { unRemainingActMatcher :: ActMatcher }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Hashable)

data DamageEffectMatcher
  = AttackDamageEffect
  | NonAttackDamageEffect
  | AnyDamageEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EnemyAttackMatcher
  = AnyEnemyAttack
  | AttackOfOpportunityAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ScenarioMatcher = TheScenario
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CampaignMatcher = TheCampaign
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data EffectMatcher = AnyEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SkillTypeMatcher = AnySkillType | NotSkillType SkillType | IsSkillType SkillType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

replaceYourLocation
  :: InvestigatorId -> (Maybe LocationId) -> LocationMatcher -> LocationMatcher
replaceYourLocation _ Nothing = id
replaceYourLocation iid (Just lid) = go
 where
  go matcher = case matcher of
    IsIchtacasDestination{} -> matcher
    LocationWithoutClues{} -> matcher
    LocationWithTitle{} -> matcher
    LocationWithFullTitle{} -> matcher
    LocationWithUnrevealedTitle{} -> matcher
    LocationWithId{} -> matcher
    LocationWithLabel{} -> matcher
    LocationWithSymbol{} -> matcher
    LocationLeavingPlay -> matcher
    LocationWithDoom{} -> matcher
    YourLocation -> LocationWithId lid
    SameLocation -> LocationWithId lid
    NotYourLocation -> NotLocation (LocationWithId lid)
    LocationIs{} -> matcher
    Anywhere -> matcher
    Nowhere -> matcher
    EmptyLocation -> matcher
    AccessibleLocation -> AccessibleFrom (LocationWithId lid)
    ConnectedFrom m -> ConnectedFrom (go m)
    ConnectedTo m -> ConnectedTo (go m)
    AccessibleFrom m -> AccessibleFrom (go m)
    AccessibleTo m -> AccessibleTo (go m)
    ConnectedLocation -> ConnectedFrom (LocationWithId lid)
    LocationWithDistanceFrom int m -> LocationWithDistanceFrom int (go m)
    LocationWithResources{} -> matcher
    LocationWithClues{} -> matcher
    LocationWithHorror{} -> matcher
    LocationWithMostClues m -> LocationWithMostClues (go m)
    LocationWithEnemy{} -> matcher
    LocationWithAsset{} -> matcher
    LocationWithInvestigator m ->
      LocationWithInvestigator (replaceYouMatcher iid m)
    RevealedLocation -> matcher
    UnrevealedLocation -> matcher
    InvestigatableLocation -> matcher
    LocationNotInPlay -> matcher
    FarthestLocationFromLocation lid' m ->
      FarthestLocationFromLocation lid' (go m)
    FarthestLocationFromYou m -> FarthestLocationFromLocation lid (go m)
    FarthestLocationFromAll m -> FarthestLocationFromAll (go m)
    NearestLocationToYou m -> NearestLocationToYou (go m) -- TODO: FIX to FromLocation
    LocationWithTrait{} -> matcher
    LocationWithoutTrait{} -> matcher
    LocationInDirection dir m -> LocationInDirection dir (go m)
    LocationWithTreachery{} -> matcher
    LocationWithoutTreachery{} -> matcher
    LocationWithoutModifier{} -> matcher
    LocationWithModifier{} -> matcher
    LocationMatchAll ms -> LocationMatchAll $ map go ms
    LocationMatchAny ms -> LocationMatchAny $ map go ms
    FirstLocation ms -> FirstLocation $ map go ms
    NotLocation m -> NotLocation (go m)
    LocationCanBeFlipped -> matcher
    ClosestPathLocation{} -> matcher
    BlockedLocation -> matcher
    ThisLocation -> matcher

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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultRemoveDoomMatchers :: RemoveDoomMatchers
defaultRemoveDoomMatchers = RemoveDoomMatchers
  { removeDoomLocations = Anywhere
  , removeDoomInvestigators = Anyone
  , removeDoomEnemies = AnyEnemy
  , removeDoomAssets = AnyAsset
  , removeDoomActs = AnyAct
  , removeDoomAgendas = AnyAgenda
  , removeDoomTreacheries = AnyTreachery
  , removeDoomEvents = AnyEvent
  , removeDoomSkills = AnySkill
  }
