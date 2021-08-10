{-# LANGUAGE PatternSynonyms #-}

module Arkham.Types.Matcher where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardType
import Arkham.Types.ClassSymbol
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

type Who = InvestigatorMatcher

data InvestigatorMatcher
  = InvestigatorAtYourLocation
  | You
  | NotYou
  | Anyone
  | InvestigatorCanMove
  | InvestigatorWithDamage
  | InvestigatorWithHorror
  | InvestigatorWithId InvestigatorId
  | InvestigatorMatches [InvestigatorMatcher]
  | TurnInvestigator
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  | AssetWithUseType UseType
  | AssetWithUses UseType
  | AssetIs CardCode
  | AnyAsset
  | EnemyAsset EnemyId
  | AssetAt LocationMatcher
  | DiscardableAsset
  | AssetWithDamage
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

pattern HunterEnemy :: EnemyMatcher
pattern HunterEnemy <- EnemyWithKeyword Keyword.Hunter where
  HunterEnemy = EnemyWithKeyword Keyword.Hunter

pattern EliteEnemy :: EnemyMatcher
pattern EliteEnemy <- EnemyWithTrait Elite where
  EliteEnemy = EnemyWithTrait Elite

pattern NonEliteEnemy :: EnemyMatcher
pattern NonEliteEnemy <- EnemyWithoutTrait Elite where
  NonEliteEnemy = EnemyWithoutTrait Elite

data EnemyMatcher
  = EnemyWithTitle Text
  | EnemyWithFullTitle Text Text
  | EnemyWithId EnemyId
  | EnemyWithTrait Trait
  | EnemyWithoutTrait Trait
  | EnemyWithKeyword Keyword
  | AnyEnemy
  | ExhaustedEnemy
  | NonWeaknessEnemy
  | EnemyAtYourLocation
  | EnemyAtLocation LocationId
  | EnemyMatchAll [EnemyMatcher]
  | EnemyEngagedWithYou
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup EnemyMatcher where
  EnemyMatchAll xs <> EnemyMatchAll ys = EnemyMatchAll (xs <> ys)
  EnemyMatchAll xs <> x = EnemyMatchAll (x : xs)
  x <> EnemyMatchAll xs = EnemyMatchAll (x : xs)
  x <> y = EnemyMatchAll [x, y]

data EventMatcher
  = EventWithTitle Text
  | EventWithFullTitle Text Text
  | EventWithId EventId
  | EventWithTrait Trait
  | EventWithClass ClassSymbol
  | EventOwnedBy InvestigatorId
  | EventMatches [EventMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

type Where = LocationMatcher

pattern LocationWithoutTreachery :: CardCode -> LocationMatcher
pattern LocationWithoutTreachery card <-
  LocationWithoutTreacheryWithCardCode card where
  LocationWithoutTreachery card =
    LocationWithoutTreacheryWithCardCode (toCardCode card)

data LocationMatcher
  = LocationWithTitle Text
  | LocationWithFullTitle Text Text
  | LocationWithId LocationId
  | LocationWithLabel Text
  | YourLocation
  | NotYourLocation
  | Anywhere
  | EmptyLocation
  | AccessibleLocation
  | AccessibleFrom LocationId
  | ConnectedLocation
  | LocationWithClues
  | LocationWithoutInvestigators
  | LocationWithoutEnemies
  | RevealedLocation
  | InvestigatableLocation
  | FarthestLocationFromYou LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTreacheryWithCardCode CardCode
  | LocationMatchers [LocationMatcher]
  | FirstLocation [LocationMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup LocationMatcher where
  LocationMatchers xs <> LocationMatchers ys = LocationMatchers $ xs <> ys
  LocationMatchers xs <> x = LocationMatchers (x : xs)
  x <> LocationMatchers xs = LocationMatchers (x : xs)
  x <> y = LocationMatchers [x, y]

data SkillMatcher
  = SkillWithTitle Text
  | SkillWithFullTitle Text Text
  | SkillWithId SkillId
  | SkillWithTrait Trait
  | SkillWithClass ClassSymbol
  | SkillOwnedBy InvestigatorId
  | SkillMatches [SkillMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup SkillMatcher where
  SkillMatches xs <> SkillMatches ys = SkillMatches (xs <> ys)
  SkillMatches xs <> x = SkillMatches (x : xs)
  x <> SkillMatches xs = SkillMatches (x : xs)
  x <> y = SkillMatches [x, y]

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithTrait Trait
  | TreacheryOwnedBy InvestigatorId
  | TreacheryMatches [TreacheryMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup TreacheryMatcher where
  TreacheryMatches xs <> TreacheryMatches ys = TreacheryMatches (xs <> ys)
  TreacheryMatches xs <> x = TreacheryMatches (x : xs)
  x <> TreacheryMatches xs = TreacheryMatches (x : xs)
  x <> y = TreacheryMatches [x, y]

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

-- | Only relies on card state, can be used purely with `cardMatch`
data CardMatcher
  = CardWithType CardType
  | CardWithCardCode CardCode
  | CardWithTitle Text
  | CardWithTrait Trait
  | CardWithoutKeyword Keyword
  | CardWithClass ClassSymbol
  | CardWithOneOf [CardMatcher]
  | CardMatches [CardMatcher]
  | IsEncounterCard
  | NonWeakness
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

data WindowMatcher
  = EnemyDefeated When Who EnemyMatcher
  | EnemyEvaded When Who EnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | EnemyAttacks When Who EnemyMatcher
  | RevealChaosToken When Who WindowTokenMatcher
  | SkillTestResult When Who SkillTestMatcher SkillTestResultMatcher
  | WhenWouldHaveSkillTestResult Who SkillTestMatcher SkillTestResultMatcher
  | WhenEnemySpawns Where EnemyMatcher
  | FastPlayerWindow
  | AfterTurnBegins Who
  | DuringTurn Who
  | OrWindowMatcher [WindowMatcher]
  | DealtDamageOrHorror Who
  | DrawCard When Who CardMatcher
  | PhaseBegins When WindowPhaseMatcher
  | PlayerHasPlayableCard ExtendedCardMatcher
  | WhenAssetEntersPlay AssetMatcher
  | Enters When Who Where
  | AnyWindow
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type When = WindowTimingMatcher

data WindowTimingMatcher = When | After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SkillTestMatcher = WhileInvestigating | WhileAttackingAnEnemy | AnySkillTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SkillTestResultMatcher = FailureResult ValueMatcher | SuccessResult ValueMatcher | AnyResult
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ValueMatcher = LessThan (GameValue Int) | AnyValue
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowTokenMatcher = WithNegativeModifier
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowPhaseMatcher = AnyPhase
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMythosStepMatcher = WhenAllDrawEncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
