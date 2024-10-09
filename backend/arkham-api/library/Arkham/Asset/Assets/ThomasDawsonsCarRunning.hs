module Arkham.Asset.Assets.ThomasDawsonsCarRunning (
  thomasDawsonsCarRunning,
  ThomasDawsonsCarRunning (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Vehicle
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name
import Arkham.Placement

newtype ThomasDawsonsCarRunning = ThomasDawsonsCarRunning AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasDawsonsCarRunning :: AssetCard ThomasDawsonsCarRunning
thomasDawsonsCarRunning = asset ThomasDawsonsCarRunning Cards.thomasDawsonsCarRunning

instance HasAbilities ThomasDawsonsCarRunning where
  getAbilities (ThomasDawsonsCarRunning x) =
    [ vehicleEnterOrExitAbility x
    ]

instance RunMessage ThomasDawsonsCarRunning where
  runMessage msg a@(ThomasDawsonsCarRunning attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    PlaceInvestigator iid (InVehicle aid) | aid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . ThomasDawsonsCarRunning $ attrs' & driverL %~ Just . fromMaybe iid
    PlaceInvestigator iid _ | Just iid == attrs.driver -> do
      attrs' <- liftRunMessage msg attrs
      passengers <- select $ InVehicleMatching (be attrs)
      for_ (headMay passengers) \p -> do
        chooseOrRunOneM p do
          questionLabeled $ "Choose new driver for " <> toTitle attrs.name
          targets passengers $ push . SetDriver attrs.id
      pure . ThomasDawsonsCarRunning $ attrs' & driverL .~ Nothing
    _ -> ThomasDawsonsCarRunning <$> liftRunMessage msg attrs
