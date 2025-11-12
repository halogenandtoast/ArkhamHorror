module Arkham.Location.Cards.SlotMachines (slotMachines) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Data.Function (on)

newtype SlotMachines = SlotMachines LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slotMachines :: LocationCard SlotMachines
slotMachines = symbolLabel $ location SlotMachines Cards.slotMachines 2 (PerPlayer 1)

instance HasAbilities SlotMachines where
  getAbilities (SlotMachines a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (ResourceCost 1)

instance RunMessage SlotMachines where
  runMessage msg l@(SlotMachines attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid NoMulligan 3
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      let hand = sortBy (compare `on` toPlayingCard) $ filter (isJust . toPlayingCard) cards
      when (allSameSuit cards || allSameRank cards) $ winGame iid attrs 3
      focusCards hand $ continue_ iid
      pure l
    _ -> SlotMachines <$> liftRunMessage msg attrs
