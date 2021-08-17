{-# LANGUAGE PatternSynonyms #-}

module Arkham.Types.Criteria where

import Arkham.Prelude

import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Trait

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AnyHorrorOnThis :: Criterion
pattern AnyHorrorOnThis <- HorrorOnThis (GreaterThan (Static 0)) where
  AnyHorrorOnThis = HorrorOnThis (GreaterThan (Static 0))

data Criterion
  = AnotherInvestigatorInSameLocation
  | InvestigatorIsAlone
  | ScenarioCardHasResignAbility
  | ClueOnLocation
  | Self
  | DuringTurn InvestigatorMatcher
  | FirstAction
  | OnLocation LocationMatcher
  | CardExists CardMatcher
  | ExtendedCardExists ExtendedCardMatcher
  | PlayableCardExists ExtendedCardMatcher
  | AssetExists AssetMatcher
  | InvestigatorExists InvestigatorMatcher
  | EnemyExists EnemyMatcher
  | SetAsideCardExists CardMatcher
  | NoEnemyExists EnemyMatcher
  | LocationExists LocationMatcher
  | InvestigatorsHaveSpendableClues ValueMatcher
  | CluesOnThis ValueMatcher
  | HorrorOnThis ValueMatcher
  | OwnCardWithDoom
  | OwnsThis
  | OnSameLocation
  | Unowned
  | DuringSkillTest SkillTestMatcher
  | InThreatAreaOf InvestigatorMatcher
  | CardInDiscard DiscardSignifier [Trait]
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | Criteria [Criterion]
  | AnyCriterion [Criterion]
  | NoRestriction
  | Never
  | Negate Criterion
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup Criterion where
  Never <> _ = Never
  _ <> Never = Never
  NoRestriction <> x = x
  x <> NoRestriction = x
  Criteria xs <> Criteria ys = Criteria $ xs <> ys
  Criteria xs <> x = Criteria $ x : xs
  x <> Criteria xs = Criteria $ x : xs
  x <> y = Criteria [x, y]

instance Monoid Criterion where
  mempty = NoRestriction
