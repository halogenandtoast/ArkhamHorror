module Arkham.Asset.Assets.DrDewiIrawanCryptozoologist (drDewiIrawanCryptozoologist) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DrDewiIrawanCryptozoologist = DrDewiIrawanCryptozoologist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drDewiIrawanCryptozoologist :: AssetCard DrDewiIrawanCryptozoologist
drDewiIrawanCryptozoologist = asset DrDewiIrawanCryptozoologist Cards.drDewiIrawanCryptozoologist

instance RunMessage DrDewiIrawanCryptozoologist where
  runMessage msg (DrDewiIrawanCryptozoologist attrs) = runQueueT $ case msg of
    _ -> DrDewiIrawanCryptozoologist <$> liftRunMessage msg attrs
