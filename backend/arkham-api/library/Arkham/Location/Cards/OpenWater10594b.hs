module Arkham.Location.Cards.OpenWater10594b (openWater10594b) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10594b = OpenWater10594b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10594b :: LocationCard OpenWater10594b
openWater10594b = locationWith OpenWater10594b Cards.openWater10594b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10594b where
  getAbilities (OpenWater10594b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10594b where
  runMessage msg l@(OpenWater10594b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> OpenWater10594b <$> liftRunMessage msg attrs
