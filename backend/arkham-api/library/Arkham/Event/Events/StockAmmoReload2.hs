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
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 5 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      when (n > 0) do
        ammoAssets <- select $ assetControlledBy iid <> #firearm <> AssetCanHaveUses Ammo
        unless (null ammoAssets) do
          chooseOrRunTargetM iid ammoAssets \asset ->
            addUses attrs asset Ammo 1
          doStep (n - 1) msg'
      pure e
    _ -> StockAmmoReload2 <$> liftRunMessage msg attrs
