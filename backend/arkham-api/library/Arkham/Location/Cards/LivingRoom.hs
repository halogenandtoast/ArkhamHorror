module Arkham.Location.Cards.LivingRoom (livingRoom) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LivingRoom = LivingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

livingRoom :: LocationCard LivingRoom
livingRoom = location LivingRoom Cards.livingRoom 3 (Static 0)

instance HasAbilities LivingRoom where
  getAbilities (LivingRoom a) =
    extendRevealed1 a
      $ groupLimit PerPhase
      $ restricted a 1 (Here <> youExist can.draw.cards)
      $ freeReaction (PerformAction #after You #parley)

instance RunMessage LivingRoom where
  runMessage msg l@(LivingRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> LivingRoom <$> liftRunMessage msg attrs
