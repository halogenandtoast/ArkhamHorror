module Arkham.Asset.Assets.HigherEducation3 (higherEducation3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype HigherEducation3 = HigherEducation3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation3 :: AssetCard HigherEducation3
higherEducation3 = asset HigherEducation3 Cards.higherEducation3

instance HasAbilities HigherEducation3 where
  getAbilities (HigherEducation3 x) =
    [ (cardI18n $ withI18nTooltip "higherEducation3.fastSpend1ResourceYouGet2WillpowerForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlledAbility x 1 restriction (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "higherEducation3.fastSpend1ResourceYouGet2IntellectForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlledAbility x 2 restriction (FastAbility $ ResourceCost 1)
    ]
   where
    restriction = DuringAnySkillTest <> youExist (HandWith $ LengthIs $ atLeast 5)

instance RunMessage HigherEducation3 where
  runMessage msg a@(HigherEducation3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #intellect 2)
      pure a
    _ -> HigherEducation3 <$> liftRunMessage msg attrs
