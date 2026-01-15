module Arkham.Asset.Assets.FingerprintKit (fingerprintKit) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype FingerprintKit = FingerprintKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit :: AssetCard FingerprintKit
fingerprintKit = asset FingerprintKit Cards.fingerprintKit

instance HasAbilities FingerprintKit where
  getAbilities (FingerprintKit a) =
    [investigateAbility a 1 (exhaust a <> assetUseCost a Supply 1) ControlsThis]

instance RunMessage FingerprintKit where
  runMessage msg a@(FingerprintKit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #intellect 1, DiscoveredClues 1]
      investigate sid iid (attrs.ability 1)
      pure a
    _ -> FingerprintKit <$> liftRunMessage msg attrs
