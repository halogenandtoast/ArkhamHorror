module Arkham.Location.Cards.DimlyLitRoad_c (dimlyLitRoad_c, DimlyLitRoad_c (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype DimlyLitRoad_c = DimlyLitRoad_c LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimlyLitRoad_c :: LocationCard DimlyLitRoad_c
dimlyLitRoad_c =
  locationWith DimlyLitRoad_c Cards.dimlyLitRoad_c 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DimlyLitRoad_c where
  getAbilities (DimlyLitRoad_c a) =
    extendRevealed a [mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)]

instance RunMessage DimlyLitRoad_c where
  runMessage msg l@(DimlyLitRoad_c attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    _ -> DimlyLitRoad_c <$> liftRunMessage msg attrs
