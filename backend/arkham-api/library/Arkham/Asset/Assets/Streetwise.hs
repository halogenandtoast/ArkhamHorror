module Arkham.Asset.Assets.Streetwise (streetwise, Streetwise (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Streetwise = Streetwise AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetwise :: AssetCard Streetwise
streetwise = asset Streetwise Cards.streetwise

instance HasAbilities Streetwise where
  getAbilities (Streetwise a) =
    [ (cardI18n $ withI18nTooltip "streetwise.fastSpend2ResourcesYouGet2IntellectForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlledAbility a 1 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    , (cardI18n $ withI18nTooltip "streetwise.fastSpend2ResourcesYouGet2AgilityForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlledAbility a 2 DuringAnySkillTest (FastAbility $ ResourceCost 2)
    ]

instance RunMessage Streetwise where
  runMessage msg a@(Streetwise attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 2)
      pure a
    _ -> Streetwise <$> liftRunMessage msg attrs
