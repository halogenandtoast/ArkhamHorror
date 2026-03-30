module Arkham.Asset.Assets.SharpRhetoric3 (sharpRhetoric3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestAction, withSkillTest, isParley)
import Arkham.Matcher
import Arkham.Modifier

newtype SharpRhetoric3 = SharpRhetoric3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpRhetoric3 :: AssetCard SharpRhetoric3
sharpRhetoric3 = asset SharpRhetoric3 Cards.sharpRhetoric3

instance HasAbilities SharpRhetoric3 where
  getAbilities (SharpRhetoric3 a) =
    [ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage SharpRhetoric3 where
  runMessage msg a@(SharpRhetoric3 attrs) = runQueueT $ case msg of
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
    _ -> SharpRhetoric3 <$> liftRunMessage msg attrs
