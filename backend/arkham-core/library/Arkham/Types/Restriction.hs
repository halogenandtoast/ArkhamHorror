module Arkham.Types.Restriction where

import Arkham.Prelude

import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Trait

data DiscardSignifier = AnyPlayerDiscard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
  | NoEnemyExists EnemyMatcher
  | LocationExists LocationMatcher
  | OwnCardWithDoom
  | OwnsThis
  | OnSameLocation
  | Unowned
  | CardInDiscard DiscardSignifier [Trait]
  | ReturnableCardInDiscard DiscardSignifier [Trait]
  | Restrictions [Restriction]
  | AnyRestriction [Restriction]
  | NoRestriction
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
