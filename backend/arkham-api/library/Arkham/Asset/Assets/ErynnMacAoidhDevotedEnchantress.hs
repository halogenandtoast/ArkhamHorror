module Arkham.Asset.Assets.ErynnMacAoidhDevotedEnchantress (erynnMacAoidhDevotedEnchantress) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ErynnMacAoidhDevotedEnchantress = ErynnMacAoidhDevotedEnchantress AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erynnMacAoidhDevotedEnchantress :: AssetCard ErynnMacAoidhDevotedEnchantress
erynnMacAoidhDevotedEnchantress = asset ErynnMacAoidhDevotedEnchantress Cards.erynnMacAoidhDevotedEnchantress

instance RunMessage ErynnMacAoidhDevotedEnchantress where
  runMessage msg (ErynnMacAoidhDevotedEnchantress attrs) = runQueueT $ case msg of
    _ -> ErynnMacAoidhDevotedEnchantress <$> liftRunMessage msg attrs
