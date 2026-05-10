module Arkham.Asset.Assets.HigherEducation (higherEducation, HigherEducation (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype HigherEducation = HigherEducation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

higherEducation :: AssetCard HigherEducation
higherEducation = asset HigherEducation Cards.higherEducation

instance HasAbilities HigherEducation where
  getAbilities (HigherEducation x) =
    [ (cardI18n $ withI18nTooltip "higherEducation.fastSpend1Resource2")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlledAbility x 1 restriction (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "higherEducation.fastSpend1Resource")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlledAbility x 2 restriction (FastAbility $ ResourceCost 1)
    ]
   where
    restriction = DuringAnySkillTest <> youExist (HandWith $ LengthIs $ atLeast 5)

instance RunMessage HigherEducation where
  runMessage msg a@(HigherEducation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #intellect 1)
      pure a
    _ -> HigherEducation <$> liftRunMessage msg attrs
