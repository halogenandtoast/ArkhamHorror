module Arkham.Location.Cards.Ballroom
  ( ballroom
  , Ballroom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Matcher
import Arkham.Message hiding (MoveAction)
import Arkham.Timing qualified as Timing

newtype Ballroom = Ballroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ballroom :: LocationCard Ballroom
ballroom =
  location Ballroom Cards.ballroom 4 (Static 0) Square [T, Circle, Squiggle]

instance HasAbilities Ballroom where
  getAbilities (Ballroom attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
        attrs
        1
        Here
        (ReactionAbility
          (PerformAction Timing.After You $ ActionIs Action.Parley)
          Free
        )
      & abilityLimitL
      .~ GroupLimit PerPhase 1
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage Ballroom where
  runMessage msg l@(Ballroom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 2 False)
    _ -> Ballroom <$> runMessage msg attrs
