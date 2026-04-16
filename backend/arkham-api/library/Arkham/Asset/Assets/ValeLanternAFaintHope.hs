module Arkham.Asset.Assets.ValeLanternAFaintHope (valeLanternAFaintHope) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Trait (Trait (Dark))

newtype ValeLanternAFaintHope = ValeLanternAFaintHope AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeLanternAFaintHope :: AssetCard ValeLanternAFaintHope
valeLanternAFaintHope = asset ValeLanternAFaintHope Cards.valeLanternAFaintHope

instance HasModifiersFor ValeLanternAFaintHope where
  getModifiersFor (ValeLanternAFaintHope a) = do
    withLocationOf a \loc -> modified_ a loc [RemoveTrait Dark]

instance RunMessage ValeLanternAFaintHope where
  runMessage msg (ValeLanternAFaintHope attrs) = runQueueT $ case msg of
    _ -> ValeLanternAFaintHope <$> liftRunMessage msg attrs
