module Arkham.Asset.Cards.PhysicalTraining (PhysicalTraining (..), physicalTraining) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype PhysicalTraining = PhysicalTraining AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

physicalTraining :: AssetCard PhysicalTraining
physicalTraining = asset PhysicalTraining Cards.physicalTraining

instance HasAbilities PhysicalTraining where
  getAbilities (PhysicalTraining a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
        $ controlledAbility a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ skillTestModifier (attrs.ability 1) iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ skillTestModifier (attrs.ability 2) iid (SkillModifier #combat 1)
      pure a
    _ -> PhysicalTraining <$> runMessage msg attrs
