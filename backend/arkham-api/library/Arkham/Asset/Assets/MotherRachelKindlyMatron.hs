module Arkham.Asset.Assets.MotherRachelKindlyMatron (motherRachelKindlyMatron) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)

newtype MotherRachelKindlyMatron = MotherRachelKindlyMatron AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

motherRachelKindlyMatron :: AssetCard MotherRachelKindlyMatron
motherRachelKindlyMatron = asset MotherRachelKindlyMatron Cards.motherRachelKindlyMatron

instance HasAbilities MotherRachelKindlyMatron where
  getAbilities (MotherRachelKindlyMatron a) =
    [groupLimit PerGame $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage MotherRachelKindlyMatron where
  runMessage msg a@(MotherRachelKindlyMatron attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      codex iid 1
      pure a
    _ -> MotherRachelKindlyMatron <$> liftRunMessage msg attrs
