module Arkham.Asset.Assets.ValeLanternExtinguishedLightBoon (valeLanternExtinguishedLightBoon) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ValeLanternExtinguishedLightBoon = ValeLanternExtinguishedLightBoon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternExtinguishedLightBoon :: AssetCard ValeLanternExtinguishedLightBoon
valeLanternExtinguishedLightBoon = asset ValeLanternExtinguishedLightBoon Cards.valeLanternExtinguishedLightBoon

instance HasAbilities ValeLanternExtinguishedLightBoon where
  getAbilities (ValeLanternExtinguishedLightBoon a) =
    [ restricted a 1 OnSameLocation actionAbility
    , mkAbility a 2 $ forced (AssetWouldLeavePlay #when $ AssetWithId a.id)
    ]

instance RunMessage ValeLanternExtinguishedLightBoon where
  runMessage msg a@(ValeLanternExtinguishedLightBoon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfAsset iid attrs
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      withLocationOf iid $ place attrs . AtLocation
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.valeLanternBeaconOfHope
      pure a
    _ -> ValeLanternExtinguishedLightBoon <$> liftRunMessage msg attrs
