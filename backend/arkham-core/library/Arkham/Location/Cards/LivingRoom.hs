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
    withRevealedAbilities attrs
      $ [ limitedAbility (GroupLimit PerPhase 1)
            $ restrictedAbility attrs 1 Here
            $ ReactionAbility (PerformAction Timing.After You $ ActionIs Action.Parley) Free
        ]

instance RunMessage LivingRoom where
  runMessage msg l@(LivingRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure l
    _ -> LivingRoom <$> runMessage msg attrs
