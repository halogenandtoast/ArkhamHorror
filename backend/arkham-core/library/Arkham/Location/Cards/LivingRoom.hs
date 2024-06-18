module Arkham.Location.Cards.LivingRoom (livingRoom, LivingRoom (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype LivingRoom = LivingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingRoom :: LocationCard LivingRoom
livingRoom = location LivingRoom Cards.livingRoom 3 (Static 0)

instance HasAbilities LivingRoom where
  getAbilities (LivingRoom attrs) =
    withRevealedAbilities attrs
      $ [ groupLimit PerPhase
            $ restrictedAbility attrs 1 Here
            $ freeReaction (PerformAction #after You #parley)
        ]

instance RunMessage LivingRoom where
  runMessage msg l@(LivingRoom attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 1
      pure l
    _ -> LivingRoom <$> runMessage msg attrs
