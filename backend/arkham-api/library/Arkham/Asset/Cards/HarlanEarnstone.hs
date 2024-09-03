module Arkham.Asset.Cards.HarlanEarnstone (harlanEarnstone, HarlanEarnstone (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype HarlanEarnstone = HarlanEarnstone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harlanEarnstone :: AssetCard HarlanEarnstone
harlanEarnstone = asset HarlanEarnstone Cards.harlanEarnstone

instance HasAbilities HarlanEarnstone where
  getAbilities (HarlanEarnstone a) = [skillTestAbility $ restrictedAbility a 1 OnSameLocation $ parleyAction (DiscardTopOfDeckCost 3)]

instance RunMessage HarlanEarnstone where
  runMessage msg a@(HarlanEarnstone attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) attrs #willpower (Fixed 4)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ PlaceClues (attrs.ability 1) (toTarget attrs) 1
      pure a
    _ -> HarlanEarnstone <$> runMessage msg attrs
