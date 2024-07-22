module Arkham.Asset.Cards.Streetwise3 (streetwise3, Streetwise3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Prelude

newtype Streetwise3 = Streetwise3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise3 :: AssetCard Streetwise3
streetwise3 = asset Streetwise3 Cards.streetwise3

instance HasAbilities Streetwise3 where
  getAbilities (Streetwise3 a) =
    [ withTooltip "{fast} Spend 2 resources: You get +3 {intellect} for this skill test."
        $ controlledAbility a 1 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    , withTooltip "{fast} Spend 2 resources: You get +3 {agility} for this skill test."
        $ controlledAbility a 2 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    ]

instance RunMessage Streetwise3 where
  runMessage msg a@(Streetwise3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 3)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 3)
      pure a
    _ -> Streetwise3 <$> runMessage msg attrs
