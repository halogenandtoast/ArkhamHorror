module Arkham.Location.Cards.DimlyLitRoad_b (
  dimlyLitRoad_b,
  DimlyLitRoad_b (..),
)
where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype DimlyLitRoad_b = DimlyLitRoad_b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimlyLitRoad_b :: LocationCard DimlyLitRoad_b
dimlyLitRoad_b =
  locationWith DimlyLitRoad_b Cards.dimlyLitRoad_b 3 (PerPlayer 2)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities DimlyLitRoad_b where
  getAbilities (DimlyLitRoad_b a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleLeaves #when (notSeenVehicle a) (be a)
      ]

instance RunMessage DimlyLitRoad_b where
  runMessage msg l@(DimlyLitRoad_b attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 (getLeavingVehicle -> vehicle) _ -> do
      n <- handleVehicleLeaves vehicle attrs.id 2
      inVehicle <- select $ InVehicleMatching (AssetWithId vehicle)
      replicateM_ n $ chooseTargetM iid inVehicle $ handleTarget iid (attrs.ability 2)
      pure . DimlyLitRoad_b $ attrs & globalMetaL %~ sawVehicle vehicle
    HandleTargetChoice _iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid) -> do
      assignHorror iid (attrs.ability 2) 1
      pure l
    _ -> DimlyLitRoad_b <$> liftRunMessage msg attrs
