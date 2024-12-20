module Arkham.Asset.Assets.ClaypoolsFurs (claypoolsFurs) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ClaypoolsFurs = ClaypoolsFurs AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

claypoolsFurs :: AssetCard ClaypoolsFurs
claypoolsFurs = asset ClaypoolsFurs Cards.claypoolsFurs

instance RunMessage ClaypoolsFurs where
  runMessage msg (ClaypoolsFurs attrs) = runQueueT $ case msg of
    _ -> ClaypoolsFurs <$> liftRunMessage msg attrs
