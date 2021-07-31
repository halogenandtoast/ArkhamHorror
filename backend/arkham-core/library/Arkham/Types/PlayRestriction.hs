module Arkham.Types.PlayRestriction where

import Arkham.Prelude

import Arkham.Types.Matcher
import Arkham.Types.Trait

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data PlayRestriction
  = AnotherInvestigatorInSameLocation
  | ScenarioCardHasResignAbility
  | ClueOnLocation
  | FirstAction
  | EnemyExists EnemyMatcher
  | NoEnemyExists EnemyMatcher
  | LocationExists LocationMatcher
  | OwnCardWithDoom
  | CardInDiscard DiscardSignifier [Trait]
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | PlayRestrictions [PlayRestriction]
  | AnyPlayRestriction [PlayRestriction]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup PlayRestriction where
  PlayRestrictions xs <> PlayRestrictions ys = PlayRestrictions $ xs <> ys
  PlayRestrictions xs <> x = PlayRestrictions $ x : xs
  x <> PlayRestrictions xs = PlayRestrictions $ x : xs
  x <> y = PlayRestrictions [x, y]
