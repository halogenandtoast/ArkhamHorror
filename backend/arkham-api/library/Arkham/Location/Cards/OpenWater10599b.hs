module Arkham.Location.Cards.OpenWater10599b (openWater10599b) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10599b = OpenWater10599b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10599b :: LocationCard OpenWater10599b
openWater10599b = locationWith OpenWater10599b Cards.openWater10599b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10599b where
  getAbilities (OpenWater10599b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10599b where
  runMessage msg l@(OpenWater10599b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> OpenWater10599b <$> liftRunMessage msg attrs
