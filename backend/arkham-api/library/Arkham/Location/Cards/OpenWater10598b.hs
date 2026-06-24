module Arkham.Location.Cards.OpenWater10598b (openWater10598b) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10598b = OpenWater10598b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10598b :: LocationCard OpenWater10598b
openWater10598b = locationWith OpenWater10598b Cards.openWater10598b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10598b where
  getAbilities (OpenWater10598b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10598b where
  runMessage msg l@(OpenWater10598b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.rottenDock
      pure l
    _ -> OpenWater10598b <$> liftRunMessage msg attrs
