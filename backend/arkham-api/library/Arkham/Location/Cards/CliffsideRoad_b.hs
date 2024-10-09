module Arkham.Location.Cards.CliffsideRoad_b (cliffsideRoad_b, CliffsideRoad_b (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype CliffsideRoad_b = CliffsideRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsideRoad_b :: LocationCard CliffsideRoad_b
cliffsideRoad_b =
  locationWith CliffsideRoad_b Cards.cliffsideRoad_b 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities CliffsideRoad_b where
  getAbilities (CliffsideRoad_b a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage CliffsideRoad_b where
  runMessage msg l@(CliffsideRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    _ -> CliffsideRoad_b <$> liftRunMessage msg attrs
