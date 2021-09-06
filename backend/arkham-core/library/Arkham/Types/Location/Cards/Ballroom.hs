module Arkham.Types.Location.Cards.Ballroom
  ( ballroom
  , Ballroom(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (MoveAction)
import qualified Arkham.Types.Timing as Timing

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

instance LocationRunner env => RunMessage env Ballroom where
  runMessage msg l@(Ballroom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (TakeResources iid 2 False)
    _ -> Ballroom <$> runMessage msg attrs
