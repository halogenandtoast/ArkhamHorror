module Arkham.Event.Events.StockAmmoReload2 (stockAmmoReload2) where

import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype StockAmmoReload2 = StockAmmoReload2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stockAmmoReload2 :: EventCard StockAmmoReload2
stockAmmoReload2 = event StockAmmoReload2 Cards.stockAmmoReload2

instance RunMessage StockAmmoReload2 where
  runMessage msg e@(StockAmmoReload2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      ammoAssets <- select $ assetControlledBy iid <> #firearm <> AssetCanHaveUses Ammo
      case ammoAssets of
        [] -> pure ()
        [asset] -> addUses attrs asset Ammo 5
        assets -> chooseAssetAmounts iid "Distribute 5 Ammo" 5 assets attrs
      pure e
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      for_ choices \(nu, n) -> addUses attrs (AssetId nu.nuUUID) Ammo n
      pure e
    _ -> StockAmmoReload2 <$> liftRunMessage msg attrs
