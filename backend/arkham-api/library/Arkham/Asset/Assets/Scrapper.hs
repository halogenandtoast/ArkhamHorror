module Arkham.Asset.Assets.Scrapper (scrapper) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Scrapper = Scrapper AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrapper :: AssetCard Scrapper
scrapper = asset Scrapper Cards.scrapper

instance HasAbilities Scrapper where
  getAbilities (Scrapper a) =
    [ (cardI18n $ withI18nTooltip "scrapper.fastSpend1ResourceYouGet1CombatForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled a 1 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "scrapper.fastSpend1ResourceYouGet1AgilityForThisSkillTest")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 (DuringSkillTest AnySkillTest) (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Scrapper where
  runMessage msg a@(Scrapper attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid $ SkillModifier #combat 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid $ SkillModifier #agility 1
      pure a
    _ -> Scrapper <$> liftRunMessage msg attrs
