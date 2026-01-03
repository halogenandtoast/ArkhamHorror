module Arkham.Asset.Assets.LeahAtwoodTheValeCook (leahAtwoodTheValeCook) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)

newtype LeahAtwoodTheValeCook = LeahAtwoodTheValeCook AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leahAtwoodTheValeCook :: AssetCard LeahAtwoodTheValeCook
leahAtwoodTheValeCook = asset LeahAtwoodTheValeCook Cards.leahAtwoodTheValeCook

instance HasAbilities LeahAtwoodTheValeCook where
  getAbilities (LeahAtwoodTheValeCook a) =
    [groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage LeahAtwoodTheValeCook where
  runMessage msg a@(LeahAtwoodTheValeCook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid 2
      pure a
    _ -> LeahAtwoodTheValeCook <$> liftRunMessage msg attrs
