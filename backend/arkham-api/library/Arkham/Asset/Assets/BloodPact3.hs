module Arkham.Asset.Assets.BloodPact3 (bloodPact3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype BloodPact3 = BloodPact3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodPact3 :: AssetCard BloodPact3
bloodPact3 = asset BloodPact3 Cards.bloodPact3

instance HasAbilities BloodPact3 where
  getAbilities (BloodPact3 x) =
    [ (cardI18n $ withI18nTooltip "bloodPact3.fastAdd1Doom2")
        $ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled x 1 DuringYourSkillTest (FastAbility $ DoomCost (x.ability 1) (toTarget x) 1)
    , (cardI18n $ withI18nTooltip "bloodPact3.fastAdd1Doom")
        $ playerLimit PerTestOrAbility
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled x 2 DuringYourSkillTest (FastAbility $ DoomCost (x.ability 2) (toTarget x) 1)
    ]

instance RunMessage BloodPact3 where
  runMessage msg a@(BloodPact3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #willpower 3)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 3)
      pure a
    _ -> BloodPact3 <$> liftRunMessage msg attrs
