{-# LANGUAGE PatternSynonyms #-}

module Arkham.Types.Matcher where

import Arkham.Prelude

import Arkham.Types.Asset.Uses
import Arkham.Types.Card.CardCode
import Arkham.Types.ClassSymbol
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON)

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
  | ConnectedLocation
  | LocationWithClues
  | LocationWithoutInvestigators
  | LocationWithoutEnemies
  | RevealedLocation
  | FarthestLocationFromYou LocationMatcher
  | LocationWithTrait Trait
  | LocationWithoutTreacheryWithCardCode CardCode
  | LocationMatchers [LocationMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
