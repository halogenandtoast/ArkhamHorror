module Arkham.Asset.Assets.HardKnocks2 (hardKnocks2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype HardKnocks2 = HardKnocks2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hardKnocks2 :: AssetCard HardKnocks2
hardKnocks2 = asset HardKnocks2 Cards.hardKnocks2

instance HasAbilities HardKnocks2 where
  getAbilities (HardKnocks2 a) =
    [ (cardI18n $ withI18nTooltip "hardKnocks2.fastSpend1Resource2")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled a 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "hardKnocks2.fastSpend1Resource")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance RunMessage HardKnocks2 where
  runMessage msg a@(HardKnocks2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #combat 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #agility 1)
      pure a
    _ -> HardKnocks2 <$> liftRunMessage msg attrs
