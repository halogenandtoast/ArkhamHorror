module Arkham.Asset.Assets.VedaWhitsleySkilledBotanist (vedaWhitsleySkilledBotanist) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype VedaWhitsleySkilledBotanist = VedaWhitsleySkilledBotanist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vedaWhitsleySkilledBotanist :: AssetCard VedaWhitsleySkilledBotanist
vedaWhitsleySkilledBotanist = asset VedaWhitsleySkilledBotanist Cards.vedaWhitsleySkilledBotanist

instance RunMessage VedaWhitsleySkilledBotanist where
  runMessage msg (VedaWhitsleySkilledBotanist attrs) = runQueueT $ case msg of
    _ -> VedaWhitsleySkilledBotanist <$> liftRunMessage msg attrs
