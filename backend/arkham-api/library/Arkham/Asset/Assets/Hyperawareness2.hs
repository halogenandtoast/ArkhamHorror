module Arkham.Asset.Assets.Hyperawareness2 (hyperawareness2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Hyperawareness2 = Hyperawareness2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness2 :: AssetCard Hyperawareness2
hyperawareness2 = asset Hyperawareness2 Cards.hyperawareness2

instance HasAbilities Hyperawareness2 where
  getAbilities (Hyperawareness2 a) =
    [ (cardI18n $ withI18nTooltip "hyperawareness2.fastSpend1ResourceYouGet1IntellectForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlled a 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "hyperawareness2.fastSpend1ResourceYouGet1AgilityForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Hyperawareness2 where
  runMessage msg a@(Hyperawareness2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #intellect 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid attrs iid (SkillModifier #agility 1)
      pure a
    _ -> Hyperawareness2 <$> liftRunMessage msg attrs
