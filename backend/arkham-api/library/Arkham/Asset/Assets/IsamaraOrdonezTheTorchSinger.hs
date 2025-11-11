module Arkham.Asset.Assets.IsamaraOrdonezTheTorchSinger (isamaraOrdonezTheTorchSinger) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype IsamaraOrdonezTheTorchSinger = IsamaraOrdonezTheTorchSinger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

isamaraOrdonezTheTorchSinger :: AssetCard IsamaraOrdonezTheTorchSinger
isamaraOrdonezTheTorchSinger = asset IsamaraOrdonezTheTorchSinger Cards.isamaraOrdonezTheTorchSinger

instance RunMessage IsamaraOrdonezTheTorchSinger where
  runMessage msg (IsamaraOrdonezTheTorchSinger attrs) = runQueueT $ case msg of
    _ -> IsamaraOrdonezTheTorchSinger <$> liftRunMessage msg attrs
