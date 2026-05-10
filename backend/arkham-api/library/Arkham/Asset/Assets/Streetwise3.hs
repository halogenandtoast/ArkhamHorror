module Arkham.Asset.Assets.Streetwise3 (streetwise3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Streetwise3 = Streetwise3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise3 :: AssetCard Streetwise3
streetwise3 = asset Streetwise3 Cards.streetwise3

instance HasAbilities Streetwise3 where
  getAbilities (Streetwise3 a) =
    [ (cardI18n $ withI18nTooltip "streetwise3.fastSpend2ResourcesYouGet3IntellectForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlledAbility a 1 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    , (cardI18n $ withI18nTooltip "streetwise3.fastSpend2ResourcesYouGet3AgilityForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlledAbility a 2 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    ]

instance RunMessage Streetwise3 where
  runMessage msg a@(Streetwise3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 3)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 3)
      pure a
    _ -> Streetwise3 <$> liftRunMessage msg attrs
