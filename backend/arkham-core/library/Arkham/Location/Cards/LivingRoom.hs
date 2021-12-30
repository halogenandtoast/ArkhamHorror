module Arkham.Location.Cards.LivingRoom
  ( livingRoom
  , LivingRoom(..)
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

newtype LivingRoom = LivingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingRoom :: LocationCard LivingRoom
livingRoom =
  location LivingRoom Cards.livingRoom 3 (Static 0) Equals [T, Circle, Plus]

instance HasAbilities LivingRoom where
  getAbilities (LivingRoom attrs) = withBaseAbilities
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

instance LocationRunner env => RunMessage env LivingRoom where
  runMessage msg l@(LivingRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DrawCards iid 1 False)
    _ -> LivingRoom <$> runMessage msg attrs
