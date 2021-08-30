{-# LANGUAGE PatternSynonyms #-}

module Arkham.Types.Criteria where

import           Arkham.Prelude

import           Arkham.Types.GameValue
import           Arkham.Types.Matcher
import           Arkham.Types.Modifier
import           Arkham.Types.Trait

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AnyHorrorOnThis :: Criterion
pattern AnyHorrorOnThis <- HorrorOnThis (GreaterThan (Static 0)) where
  AnyHorrorOnThis = HorrorOnThis (GreaterThan (Static 0))

pattern CanGainResources :: Criterion
pattern CanGainResources <-
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotGainResources]) where
  CanGainResources = InvestigatorExists
    (InvestigatorMatches [You, InvestigatorWithoutModifier CannotGainResources])

pattern CanDrawCards :: Criterion
pattern CanDrawCards <-
  InvestigatorExists (InvestigatorMatches [You, InvestigatorWithoutModifier CannotDrawCards]) where
  CanDrawCards = InvestigatorExists
    (InvestigatorMatches [You, InvestigatorWithoutModifier CannotDrawCards])

data Criterion
  = AssetExists AssetMatcher
  | OnAct Int
  | CardExists CardMatcher
  | CardInDiscard DiscardSignifier [Trait]
  | ChargesOnThis ValueMatcher
  | ClueOnLocation
  | CluesOnThis ValueMatcher
  | DuringSkillTest SkillTestMatcher
  | DuringTurn InvestigatorMatcher
  | EnemyCriteria EnemyCriterion
  | ExtendedCardExists ExtendedCardMatcher
  | FirstAction
  | Here
  | HorrorOnThis ValueMatcher
  | InThreatAreaOf InvestigatorMatcher
  | InVictoryDisplay CardMatcher ValueMatcher
  | InvestigatorExists InvestigatorMatcher
  | InvestigatorIsAlone
  | InvestigatorsHaveSpendableClues ValueMatcher
  | LocationExists LocationMatcher
  | AllUndefeatedInvestigatorsResigned
  | OnLocation LocationMatcher
  | OnSameLocation
  | OwnCardWithDoom
  | OwnsThis
  | PlayableCardExists ExtendedCardMatcher
  | PlayableCardExistsWithCostReduction Int ExtendedCardMatcher
  | ResourcesOnThis ValueMatcher
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | ScenarioCardHasResignAbility
  | Self
  | SetAsideCardExists CardMatcher
  | Unowned
  | SelfHasModifier ModifierType
  -- Special Criterion
  | Criteria [Criterion]
  | AnyCriterion [Criterion]
  | NoRestriction
  | Never
  | Negate Criterion
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup Criterion where
  Never <> _                 = Never
  _ <> Never                 = Never
  NoRestriction <> x         = x
  x <> NoRestriction         = x
  Criteria xs <> Criteria ys = Criteria $ xs <> ys
  Criteria xs <> x           = Criteria $ x : xs
  x <> Criteria xs           = Criteria $ x : xs
  x <> y                     = Criteria [x, y]

instance Monoid Criterion where
  mempty = NoRestriction

data EnemyCriterion
  = EnemyExists EnemyMatcher
  | NotAttackingEnemy
  | EnemyExistsAtAttachedLocation EnemyMatcher
  | ThisEnemy EnemyMatcher
  | EnemyMatchesCriteria [EnemyCriterion]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup EnemyCriterion where
  EnemyMatchesCriteria xs <> EnemyMatchesCriteria ys =
    EnemyMatchesCriteria $ xs <> ys
  EnemyMatchesCriteria xs <> x = EnemyMatchesCriteria $ x : xs
  x <> EnemyMatchesCriteria xs = EnemyMatchesCriteria $ x : xs
  x <> y = EnemyMatchesCriteria [x, y]
