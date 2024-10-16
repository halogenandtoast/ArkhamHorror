module Arkham.Location.Cards.DimlyLitRoad_a (dimlyLitRoad_a, DimlyLitRoad_a (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype DimlyLitRoad_a = DimlyLitRoad_a LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimlyLitRoad_a :: LocationCard DimlyLitRoad_a
dimlyLitRoad_a =
  locationWith DimlyLitRoad_a Cards.dimlyLitRoad_a 4 (PerPlayer 1)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DimlyLitRoad_a where
  getAbilities (DimlyLitRoad_a a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage DimlyLitRoad_a where
  runMessage msg l@(DimlyLitRoad_a attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 1
      inVehicle <- select $ InVehicleMatching (AssetWithId vehicle)
      replicateM_ n $ chooseTargetM iid inVehicle $ handleTarget iid (attrs.ability 2)
      pure . DimlyLitRoad_a $ attrs & globalMetaL %~ sawVehicle vehicle
    HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid) -> do
      assignHorror iid (attrs.ability 2) 2
      pure l
    _ -> DimlyLitRoad_a <$> liftRunMessage msg attrs
