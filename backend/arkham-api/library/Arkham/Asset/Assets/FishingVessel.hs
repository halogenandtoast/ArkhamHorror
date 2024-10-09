module Arkham.Asset.Assets.FishingVessel (fishingVessel, FishingVessel (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Vehicle
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Ocean))

newtype FishingVessel = FishingVessel AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fishingVessel :: AssetCard FishingVessel
fishingVessel = asset FishingVessel Cards.fishingVessel

instance HasAbilities FishingVessel where
  getAbilities (FishingVessel x) =
    [ vehicleEnterOrExitAbility x
    , restrictedAbility x 1 InThisVehicle actionAbility
    ]

instance RunMessage FishingVessel where
  runMessage msg a@(FishingVessel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getLocationOf attrs.id >>= traverse_ \lid -> do
        oceans <- select $ withTrait Ocean <> ConnectedTo (LocationWithId lid)
        chooseTargetM iid oceans $ place attrs
      pure a
    _ -> FishingVessel <$> liftRunMessage msg attrs
