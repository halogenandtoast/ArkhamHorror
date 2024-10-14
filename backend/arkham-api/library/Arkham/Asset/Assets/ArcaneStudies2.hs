module Arkham.Asset.Assets.ArcaneStudies2 (ArcaneStudies2 (..), arcaneStudies2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype ArcaneStudies2 = ArcaneStudies2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneStudies2 :: AssetCard ArcaneStudies2
arcaneStudies2 = asset ArcaneStudies2 Cards.arcaneStudies2

instance HasAbilities ArcaneStudies2 where
  getAbilities (ArcaneStudies2 a) =
    [ withTooltip "{fast} Spend 1 resource: You get +1 {willpower} for this skill test."
        $ wantsSkillTest (YourSkillTest #willpower)
        $ controlledAbility a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip "{fast} Spend 1 resource: You get +1 {intellect} for this skill test."
        $ wantsSkillTest (YourSkillTest #intellect)
        $ controlledAbility a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage ArcaneStudies2 where
  runMessage msg a@(ArcaneStudies2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    _ -> ArcaneStudies2 <$> liftRunMessage msg attrs
