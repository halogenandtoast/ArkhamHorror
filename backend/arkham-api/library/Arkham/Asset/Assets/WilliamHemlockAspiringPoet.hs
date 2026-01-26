module Arkham.Asset.Assets.WilliamHemlockAspiringPoet (williamHemlockAspiringPoet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)

newtype WilliamHemlockAspiringPoet = WilliamHemlockAspiringPoet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

williamHemlockAspiringPoet :: AssetCard WilliamHemlockAspiringPoet
williamHemlockAspiringPoet = asset WilliamHemlockAspiringPoet Cards.williamHemlockAspiringPoet

instance HasAbilities WilliamHemlockAspiringPoet where
  getAbilities (WilliamHemlockAspiringPoet a) =
    [groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage WilliamHemlockAspiringPoet where
  runMessage msg a@(WilliamHemlockAspiringPoet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid (attrs.ability 1) 4
      pure a
    _ -> WilliamHemlockAspiringPoet <$> liftRunMessage msg attrs
