module Arkham.Location.Cards.OpenWater10595b (openWater10595b) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype OpenWater10595b = OpenWater10595b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openWater10595b :: LocationCard OpenWater10595b
openWater10595b = locationWith OpenWater10595b Cards.openWater10595b 5 (Static 0) connectsToAdjacent

instance HasAbilities OpenWater10595b where
  getAbilities (OpenWater10595b a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage OpenWater10595b where
  runMessage msg l@(OpenWater10595b attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure l
    FlipThis (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Locations.fetidPool
      pure l
    _ -> OpenWater10595b <$> liftRunMessage msg attrs
