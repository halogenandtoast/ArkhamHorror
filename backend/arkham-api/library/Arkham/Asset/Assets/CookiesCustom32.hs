module Arkham.Asset.Assets.CookiesCustom32 (cookiesCustom32) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype CookiesCustom32 = CookiesCustom32 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cookiesCustom32 :: AssetCard CookiesCustom32
cookiesCustom32 = asset CookiesCustom32 Cards.cookiesCustom32

instance RunMessage CookiesCustom32 where
  runMessage msg (CookiesCustom32 attrs) = runQueueT $ case msg of
    _ -> CookiesCustom32 <$> liftRunMessage msg attrs
