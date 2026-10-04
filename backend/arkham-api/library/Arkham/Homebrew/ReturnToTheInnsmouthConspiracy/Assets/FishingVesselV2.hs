module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.FishingVesselV2 (fishingVesselV2) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Vehicle
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ocean))

newtype FishingVesselV2 = FishingVesselV2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "While you are in this vehicle, treat your location as if it were unflooded."
instance HasModifiersFor FishingVesselV2 where
  getModifiersFor (FishingVesselV2 a) =
    modifySelect a (InVehicleMatching $ AssetWithId a.id) [TreatLocationAsUnflooded]

fishingVesselV2 :: AssetCard FishingVesselV2
fishingVesselV2 = asset FishingVesselV2 Cards.fishingVesselV2

instance HasAbilities FishingVesselV2 where
  getAbilities (FishingVesselV2 x) =
    [ vehicleEnterOrExitAbility x
    , restricted x 1 InThisVehicle actionAbility
    ]

instance RunMessage FishingVesselV2 where
  runMessage msg a@(FishingVesselV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getLocationOf attrs.id >>= traverse_ \lid -> do
        -- "If you are the only investigator in this vehicle and you are not engaged with
        -- an enemy, move to any Ocean location instead."
        alone <-
          andM
            [ (== 1) . length <$> select (InVehicleMatching $ AssetWithId attrs.id)
            , selectNone $ enemyEngagedWith iid
            ]
        oceans <-
          if alone
            then select $ LocationWithTrait Ocean
            else select $ LocationWithTrait Ocean <> connectedTo (LocationWithId lid)
        chooseTargetM iid oceans $ moveVehicle attrs lid
      pure a
    _ -> FishingVesselV2 <$> liftRunMessage msg attrs
