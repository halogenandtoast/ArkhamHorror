module Arkham.Asset.Assets.ArchibaldHudson (archibaldHudson, ArchibaldHudson(..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ArchibaldHudson = ArchibaldHudson AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archibaldHudson :: AssetCard ArchibaldHudson
archibaldHudson = allyWith ArchibaldHudson Cards.archibaldHudson (2, 2) noSlots

instance RunMessage ArchibaldHudson where
  runMessage msg (ArchibaldHudson attrs) = ArchibaldHudson <$> runMessage msg attrs
