module Arkham.Location.Cards.CliffsideRoad_a (cliffsideRoad_a, CliffsideRoad_a (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype CliffsideRoad_a = CliffsideRoad_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsideRoad_a :: LocationCard CliffsideRoad_a
cliffsideRoad_a =
  locationWith CliffsideRoad_a Cards.cliffsideRoad_a 2 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities CliffsideRoad_a where
  getAbilities (CliffsideRoad_a a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage CliffsideRoad_a where
  runMessage msg l@(CliffsideRoad_a attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    _ -> CliffsideRoad_a <$> liftRunMessage msg attrs
