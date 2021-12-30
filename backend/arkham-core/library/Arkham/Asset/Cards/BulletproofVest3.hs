module Arkham.Asset.Cards.BulletproofVest3 where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Attrs

newtype BulletproofVest3 = BulletproofVest3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

bulletproofVest3 :: AssetCard BulletproofVest3
bulletproofVest3 =
  assetWith BulletproofVest3 Cards.bulletproofVest3 (healthL ?~ 4)

instance AssetRunner env => RunMessage env BulletproofVest3 where
  runMessage msg (BulletproofVest3 attrs) =
    BulletproofVest3 <$> runMessage msg attrs
