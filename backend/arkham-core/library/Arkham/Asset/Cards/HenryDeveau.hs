module Arkham.Asset.Cards.HenryDeveau (henryDeveau, HenryDeveau (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HenryDeveau = HenryDeveau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryDeveau :: AssetCard HenryDeveau
henryDeveau = asset HenryDeveau Cards.henryDeveau

instance HasAbilities HenryDeveau where
  getAbilities (HenryDeveau a) =
    [ skillTestAbility
        $ restrictedAbility a 1 OnSameLocation
        $ parleyAction (DiscardFromCost 1 (FromHandOf You) AnyCard)
    ]

instance RunMessage HenryDeveau where
  runMessage msg a@(HenryDeveau attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ PlaceClues (attrs.ability 1) (toTarget attrs) 1
      pure a
    _ -> HenryDeveau <$> runMessage msg attrs
