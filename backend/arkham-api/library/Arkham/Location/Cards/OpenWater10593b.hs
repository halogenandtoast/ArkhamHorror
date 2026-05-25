module Arkham.Location.Cards.OpenWater10593b (openWater10593b) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10593b = OpenWater10593b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10593b :: LocationCard OpenWater10593b
openWater10593b = locationWith OpenWater10593b Cards.openWater10593b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10593b where
  getAbilities (OpenWater10593b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10593b where
  runMessage msg l@(OpenWater10593b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> OpenWater10593b <$> liftRunMessage msg attrs
