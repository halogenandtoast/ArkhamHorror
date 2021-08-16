module Arkham.Types.Criteria where

import Arkham.Prelude

import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Trait

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Criteria
  = AnotherInvestigatorInSameLocation
  | InvestigatorIsAlone
  | ScenarioCardHasResignAbility
  | ClueOnLocation
  | FirstAction
  | DuringTurn InvestigatorMatcher
  | OnLocation LocationId
  | CardExists CardMatcher
  | ExtendedCardExists ExtendedCardMatcher
  | PlayableCardExists ExtendedCardMatcher
  | AssetExists AssetMatcher
  | InvestigatorExists InvestigatorMatcher
  | EnemyExists EnemyMatcher
  | NoEnemyExists EnemyMatcher
  | LocationExists LocationMatcher
  | OwnCardWithDoom
  | Self
  | CardInDiscard DiscardSignifier [Trait]
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | Criterias [Criteria]
  | AnyCriteria [Criteria]
  | NoRestriction
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup Criteria where
  NoRestriction <> x = x
  x <> NoRestriction = x
  Criterias xs <> Criterias ys = Criterias $ xs <> ys
  Criterias xs <> x = Criterias $ x : xs
  x <> Criterias xs = Criterias $ x : xs
  x <> y = Criterias [x, y]

instance Monoid Criteria where
  mempty = NoRestriction
