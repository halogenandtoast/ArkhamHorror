module Arkham.Asset.Assets.ElinaHarpersCarRunning (
  elinaHarpersCarRunning,
  ElinaHarpersCarRunning (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Vehicle
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name
import Arkham.Placement

newtype ElinaHarpersCarRunning = ElinaHarpersCarRunning AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarpersCarRunning :: AssetCard ElinaHarpersCarRunning
elinaHarpersCarRunning = asset ElinaHarpersCarRunning Cards.elinaHarpersCarRunning

instance HasAbilities ElinaHarpersCarRunning where
  getAbilities (ElinaHarpersCarRunning x) =
    [ vehicleEnterOrExitAbility x
    ]

instance RunMessage ElinaHarpersCarRunning where
  runMessage msg a@(ElinaHarpersCarRunning attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    PlaceInvestigator iid (InVehicle aid) | aid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      pure . ElinaHarpersCarRunning $ attrs' & driverL %~ Just . fromMaybe iid
    PlaceInvestigator iid _ | Just iid == attrs.driver -> do
      attrs' <- liftRunMessage msg attrs
      passengers <- select $ InVehicleMatching (be attrs)
      for_ (headMay passengers) \p -> do
        chooseOrRunOneM p do
          questionLabeled $ "Choose new driver for " <> toTitle attrs.name
          targets passengers $ push . SetDriver attrs.id
      pure . ElinaHarpersCarRunning $ attrs' & driverL .~ Nothing
    _ -> ElinaHarpersCarRunning <$> liftRunMessage msg attrs
