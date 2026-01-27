module Arkham.Asset.Assets.SharpRhetoric (sharpRhetoric) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestAction, withSkillTest, isParley)
import Arkham.Matcher
import Arkham.Modifier

newtype SharpRhetoric = SharpRhetoric AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpRhetoric :: AssetCard SharpRhetoric
sharpRhetoric = asset SharpRhetoric Cards.sharpRhetoric

instance HasAbilities SharpRhetoric where
  getAbilities (SharpRhetoric a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {intellect} for this skill test. (+2 {intellect} instead if this is an investigation or parley)."
        $ wantsSkillTest (YourSkillTest #intellect)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {willpower} for this skill test. (+2 {willpower} instead if this is an investigation or parley)."
        $ wantsSkillTest (YourSkillTest #willpower)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage SharpRhetoric where
  runMessage msg a@(SharpRhetoric attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        maction <- getSkillTestAction
        parley <- isParley
        let n =
              if maction == Just #investigate || parley
                then 2
                else 1
        skillTestModifier sid attrs iid (SkillModifier #intellect n)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        maction <- getSkillTestAction
        parley <- isParley
        let n =
              if maction == Just #investigate || parley
                then 2
                else 1
        skillTestModifier sid attrs iid (SkillModifier #willpower n)
      pure a
    _ -> SharpRhetoric <$> liftRunMessage msg attrs
