module Arkham.Asset.Cards.BulletproofVest3 where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype BulletproofVest3 = BulletproofVest3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bulletproofVest3 :: AssetCard BulletproofVest3
bulletproofVest3 =
  assetWith BulletproofVest3 Cards.bulletproofVest3 (healthL ?~ 4)

instance RunMessage BulletproofVest3 where
  runMessage msg (BulletproofVest3 attrs) =
    BulletproofVest3 <$> runMessage msg attrs
