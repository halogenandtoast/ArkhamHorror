module Arkham.Asset.Assets.ElinaHarpersCarStopped (
  elinaHarpersCarStopped,
  ElinaHarpersCarStopped (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Vehicle
import Arkham.Matcher

newtype ElinaHarpersCarStopped = ElinaHarpersCarStopped AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elinaHarpersCarStopped :: AssetCard ElinaHarpersCarStopped
elinaHarpersCarStopped = asset ElinaHarpersCarStopped Cards.elinaHarpersCarStopped

instance HasModifiersFor ElinaHarpersCarStopped where
  getModifiersFor (ElinaHarpersCarStopped a) = do
    n <- selectCount $ InVehicleMatching (be a)
    modifySelectWhen a (n >= 2) Anyone [CannotEnterVehicle (be a)]

instance HasAbilities ElinaHarpersCarStopped where
  getAbilities (ElinaHarpersCarStopped x) =
    [ vehicleEnterOrExitAbility x
    , restrictedAbility x 2 (maybe Never (youExist . InvestigatorWithId) x.driver) actionAbility
    ]

instance RunMessage ElinaHarpersCarStopped where
  runMessage msg a@(ElinaHarpersCarStopped attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) VehicleEnterExitAbility -> do
      enterOrExitVehicle iid a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOver iid attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thomasDawsonsCarRunning
      pure a
    _ -> ElinaHarpersCarStopped <$> liftRunMessage msg attrs
