module Arkham.Asset.Assets.ValeLanternExtinguishedLight (valeLanternExtinguishedLight) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ValeLanternExtinguishedLight = ValeLanternExtinguishedLight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternExtinguishedLight :: AssetCard ValeLanternExtinguishedLight
valeLanternExtinguishedLight = asset ValeLanternExtinguishedLight Cards.valeLanternExtinguishedLight

instance HasAbilities ValeLanternExtinguishedLight where
  getAbilities (ValeLanternExtinguishedLight a) =
    [ restricted a 1 OnSameLocation freeTrigger_
    , mkAbility a 2 $ forced (AssetWouldLeavePlay #when $ AssetWithId a.id)
    ]

instance RunMessage ValeLanternExtinguishedLight where
  runMessage msg a@(ValeLanternExtinguishedLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfAsset iid attrs
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      withLocationOf iid $ place attrs . AtLocation
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.valeLanternAFaintHope
      pure a
    _ -> ValeLanternExtinguishedLight <$> liftRunMessage msg attrs
