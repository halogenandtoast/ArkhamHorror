module Arkham.Location.Cards.Ballroom
  ( ballroom
  , Ballroom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( MoveAction )
import Arkham.Timing qualified as Timing

newtype Ballroom = Ballroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ballroom :: LocationCard Ballroom
ballroom = location Ballroom Cards.ballroom 4 (Static 0)

instance HasAbilities Ballroom where
  getAbilities (Ballroom attrs) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerPhase 1)
      $ restrictedAbility attrs 1 Here
      $ ReactionAbility
          (PerformAction Timing.After You $ ActionIs Action.Parley)
          Free
    | locationRevealed attrs
    ]

instance RunMessage Ballroom where
  runMessage msg l@(Ballroom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (TakeResources iid 2 (toAbilitySource attrs 1) False)
    _ -> Ballroom <$> runMessage msg attrs
