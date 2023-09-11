module Arkham.Asset.Cards.ArcaneStudies (
  ArcaneStudies (..),
  arcaneStudies,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype ArcaneStudies = ArcaneStudies AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies :: AssetCard ArcaneStudies
arcaneStudies = asset ArcaneStudies Cards.arcaneStudies

instance HasAbilities ArcaneStudies where
  getAbilities (ArcaneStudies a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
        $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    ]

instance RunMessage ArcaneStudies where
  runMessage msg a@(ArcaneStudies attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier attrs iid (SkillModifier #willpower 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ skillTestModifier attrs iid (SkillModifier #intellect 1)
      pure a
    _ -> ArcaneStudies <$> runMessage msg attrs
