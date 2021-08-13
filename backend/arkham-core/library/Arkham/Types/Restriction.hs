{-# LANGUAGE PatternSynonyms #-}

module Arkham.Types.Restriction
  ( module Arkham.Types.Restriction
  , module X
  ) where

import Arkham.Prelude

import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher as X
import Arkham.Types.Trait

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

pattern AnyHorrorOnThis :: Restriction
pattern AnyHorrorOnThis <- HorrorOnThis (GreaterThan (Static 0)) where
  AnyHorrorOnThis = HorrorOnThis (GreaterThan (Static 0))

data Restriction
  = AnotherInvestigatorInSameLocation
  | InvestigatorIsAlone
  | ScenarioCardHasResignAbility
  | ClueOnLocation
  | FirstAction
  | OnLocation LocationId
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
  | Restrictions [Restriction]
  | AnyRestriction [Restriction]
  | NoRestriction
  | Never
  | Negate Restriction
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup Restriction where
  NoRestriction <> x = x
  x <> NoRestriction = x
  Restrictions xs <> Restrictions ys = Restrictions $ xs <> ys
  Restrictions xs <> x = Restrictions $ x : xs
  x <> Restrictions xs = Restrictions $ x : xs
  x <> y = Restrictions [x, y]

instance Monoid Restriction where
  mempty = NoRestriction
