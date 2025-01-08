module Arkham.Asset.Assets.MineralSpecimen (mineralSpecimen) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted

newtype MineralSpecimen = MineralSpecimen AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mineralSpecimen :: AssetCard MineralSpecimen
mineralSpecimen = asset MineralSpecimen Cards.mineralSpecimen

instance HasAbilities MineralSpecimen where
  getAbilities (MineralSpecimen a) = [storyControlled_ a 1 $ investigateAction $ assetUseCost a Charge 1]

instance RunMessage MineralSpecimen where
  runMessage msg a@(MineralSpecimen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      skillTestModifiers sid (attrs.ability 1) iid [BaseSkillOf #intellect 5, DiscoveredClues 1]
      skillTestModifier sid (attrs.ability 1) sid (CancelAnyChaosToken #frost)
      investigate_ sid iid (attrs.ability 1)
      pure a
    _ -> MineralSpecimen <$> liftRunMessage msg attrs
