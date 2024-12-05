module Arkham.Asset.Assets.ThomasDawsonsCarStopped (
  thomasDawsonsCarStopped,
  ThomasDawsonsCarStopped (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Vehicle
import Arkham.Matcher

newtype ThomasDawsonsCarStopped = ThomasDawsonsCarStopped AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasDawsonsCarStopped :: AssetCard ThomasDawsonsCarStopped
thomasDawsonsCarStopped = asset ThomasDawsonsCarStopped Cards.thomasDawsonsCarStopped

instance HasModifiersFor ThomasDawsonsCarStopped where
  getModifiersFor (ThomasDawsonsCarStopped a) = do
    n <- selectCount $ InVehicleMatching (be a)
    modifySelectWhen a (n >= 2) Anyone [CannotEnterVehicle (be a)]

instance HasAbilities ThomasDawsonsCarStopped where
  getAbilities (ThomasDawsonsCarStopped x) =
    [ vehicleEnterOrExitAbility x
    , restrictedAbility x 2 (maybe Never (youExist . InvestigatorWithId) x.driver) actionAbility
    ]

instance RunMessage ThomasDawsonsCarStopped where
  runMessage msg a@(ThomasDawsonsCarStopped attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOver iid attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thomasDawsonsCarRunning
      pure a
    _ -> ThomasDawsonsCarStopped <$> liftRunMessage msg attrs
