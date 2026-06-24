module Arkham.Location.Cards.OpenWater10597b (openWater10597b) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10597b = OpenWater10597b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10597b :: LocationCard OpenWater10597b
openWater10597b = locationWith OpenWater10597b Cards.openWater10597b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10597b where
  getAbilities (OpenWater10597b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10597b where
  runMessage msg l@(OpenWater10597b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.abandonedShack
      pure l
    _ -> OpenWater10597b <$> liftRunMessage msg attrs
