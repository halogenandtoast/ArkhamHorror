module Arkham.Asset.Assets.ThomasDawsonsCarRunning (
  thomasDawsonsCarRunning,
  ThomasDawsonsCarRunning (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
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
    [ restrictedAbility x 2 (maybe Never (youExist . InvestigatorWithId) x.driver) actionAbility
    ]

instance RunMessage ThomasDawsonsCarRunning where
  runMessage msg a@(ThomasDawsonsCarRunning attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOver iid attrs
      pure a
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
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thomasDawsonsCarStopped
      pure a
    _ -> ThomasDawsonsCarRunning <$> liftRunMessage msg attrs
