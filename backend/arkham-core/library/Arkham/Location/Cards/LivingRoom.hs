module Arkham.Location.Cards.LivingRoom (
  livingRoom,
  LivingRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LivingRoom = LivingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingRoom :: LocationCard LivingRoom
livingRoom = location LivingRoom Cards.livingRoom 3 (Static 0)

instance HasAbilities LivingRoom where
  getAbilities (LivingRoom attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (GroupLimit PerPhase 1) $
        restrictedAbility attrs 1 Here $
          ReactionAbility
            (PerformAction Timing.After You $ ActionIs Action.Parley)
            Free
      | locationRevealed attrs
      ]

instance RunMessage LivingRoom where
  runMessage msg l@(LivingRoom attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure l
    _ -> LivingRoom <$> runMessage msg attrs
