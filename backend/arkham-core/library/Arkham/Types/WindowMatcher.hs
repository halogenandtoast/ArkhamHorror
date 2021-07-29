{-# LANGUAGE PatternSynonyms #-}
module Arkham.Types.WindowMatcher where

import Arkham.Prelude

import Arkham.Types.Card.CardType
import Arkham.Types.GameValue
import Arkham.Types.Keyword
import Arkham.Types.Trait

data WindowMatcher
  = EnemyDefeated When Who WindowEnemyMatcher
  | EnemyEvaded When Who WindowEnemyMatcher
  | MythosStep WindowMythosStepMatcher
  | EnemyAttacks When Who WindowEnemyMatcher
  | RevealChaosToken When Who WindowTokenMatcher
  | SkillTestResult When Who SkillTestMatcher SkillTestResultMatcher
  | WhenWouldHaveSkillTestResult Who SkillTestMatcher SkillTestResultMatcher
  | WhenEnemySpawns Where WindowEnemyMatcher
  | FastPlayerWindow
  | AfterTurnBegins Who
  | DuringTurn Who
  | OrWindowMatcher [WindowMatcher]
  | DealtDamageOrHorror Who
  | DrawCard When Who WindowCardMatcher
  | PhaseBegins When WindowPhaseMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type When = WindowTimingMatcher

data WindowTimingMatcher = When | After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern NonWeaknessTreachery :: WindowCardMatcher
pattern NonWeaknessTreachery =
  CardMatches [NonWeakness, WithCardType TreacheryType]

pattern NonPeril :: WindowCardMatcher
pattern NonPeril <- CardWithoutKeyword Peril where
  NonPeril = CardWithoutKeyword Peril

data WindowCardMatcher = NonWeakness | WithCardType CardType | CardMatchesAny [WindowCardMatcher] | CardMatches [WindowCardMatcher] | CardWithoutKeyword Keyword | AnyCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup WindowCardMatcher where
  AnyCard <> a = a
  a <> AnyCard = a
  CardMatches xs <> CardMatches ys = CardMatches (xs <> ys)
  CardMatches xs <> x = CardMatches (x : xs)
  x <> CardMatches xs = CardMatches (x : xs)
  x <> y = CardMatches [x, y]

pattern EliteEnemy :: WindowEnemyMatcher
pattern EliteEnemy <- EnemyWithTrait Elite where
  EliteEnemy = EnemyWithTrait Elite

pattern NonEliteEnemy :: WindowEnemyMatcher
pattern NonEliteEnemy <- EnemyWithoutTrait Elite where
  NonEliteEnemy = EnemyWithoutTrait Elite

data WindowEnemyMatcher
  = EnemyWithoutTrait Trait
  | EnemyWithTrait Trait
  | AnyEnemy
  | NonWeaknessEnemy
  | EnemyAtYourLocation
  | EnemyMatchers [WindowEnemyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup WindowEnemyMatcher where
  EnemyMatchers xs <> EnemyMatchers ys = EnemyMatchers (xs <> ys)
  EnemyMatchers xs <> x = EnemyMatchers (x : xs)
  x <> EnemyMatchers xs = EnemyMatchers (x : xs)
  x <> y = EnemyMatchers [x, y]

type Who = WindowInvestigatorMatcher

data WindowInvestigatorMatcher = You | Anyone | InvestigatorAtYourLocation | NotYou
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

type Where = WindowLocationMatcher

data WindowLocationMatcher = YourLocation | Anywhere
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
