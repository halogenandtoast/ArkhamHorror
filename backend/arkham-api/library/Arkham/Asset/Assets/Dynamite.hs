module Arkham.Asset.Assets.Dynamite (dynamite) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype Dynamite = Dynamite AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamite :: AssetCard Dynamite
dynamite = asset Dynamite Cards.dynamite

instance RunMessage Dynamite where
  runMessage msg (Dynamite attrs) = runQueueT $ case msg of
    _ -> Dynamite <$> liftRunMessage msg attrs
