module Arkham.Asset.Cards.Hyperawareness (
  Hyperawareness (..),
  hyperawareness,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype Hyperawareness = Hyperawareness AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness :: AssetCard Hyperawareness
hyperawareness = asset Hyperawareness Cards.hyperawareness

instance HasAbilities Hyperawareness where
  getAbilities (Hyperawareness a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
        $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage Hyperawareness where
  runMessage msg a@(Hyperawareness attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #intellect 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ skillTestModifier (toAbilitySource attrs 2) iid (SkillModifier #agility 1)
      pure a
    _ -> Hyperawareness <$> runMessage msg attrs
