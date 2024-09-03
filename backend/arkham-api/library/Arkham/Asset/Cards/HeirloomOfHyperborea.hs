module Arkham.Asset.Cards.HeirloomOfHyperborea where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype HeirloomOfHyperborea = HeirloomOfHyperborea AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heirloomOfHyperborea :: AssetCard HeirloomOfHyperborea
heirloomOfHyperborea = asset HeirloomOfHyperborea Cards.heirloomOfHyperborea

instance HasAbilities HeirloomOfHyperborea where
  getAbilities (HeirloomOfHyperborea x) =
    [controlledAbility x 1 CanDrawCards $ freeReaction $ Matcher.PlayCard #after You #spell]

instance RunMessage HeirloomOfHyperborea where
  runMessage msg a@(HeirloomOfHyperborea attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> HeirloomOfHyperborea <$> runMessage msg attrs
