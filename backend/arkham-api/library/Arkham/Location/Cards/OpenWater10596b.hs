module Arkham.Location.Cards.OpenWater10596b (openWater10596b) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10596b = OpenWater10596b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10596b :: LocationCard OpenWater10596b
openWater10596b = locationWith OpenWater10596b Cards.openWater10596b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10596b where
  getAbilities (OpenWater10596b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10596b where
  runMessage msg l@(OpenWater10596b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> OpenWater10596b <$> liftRunMessage msg attrs
