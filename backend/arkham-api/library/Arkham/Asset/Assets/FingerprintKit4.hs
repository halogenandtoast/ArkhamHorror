module Arkham.Asset.Assets.FingerprintKit4 (fingerprintKit4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype FingerprintKit4 = FingerprintKit4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit4 :: AssetCard FingerprintKit4
fingerprintKit4 = asset FingerprintKit4 Cards.fingerprintKit4

instance HasAbilities FingerprintKit4 where
  getAbilities (FingerprintKit4 a) =
    [investigateAbility a 1 (exhaust a <> assetUseCost a Supply 1) ControlsThis]

instance RunMessage FingerprintKit4 where
  runMessage msg a@(FingerprintKit4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [SkillModifier #intellect 2, DiscoveredClues 2]
      investigate sid iid (attrs.ability 1)
      pure a
    _ -> FingerprintKit4 <$> liftRunMessage msg attrs
