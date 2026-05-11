module Arkham.Asset.Assets.Hyperawareness (hyperawareness) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Hyperawareness = Hyperawareness AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hyperawareness :: AssetCard Hyperawareness
hyperawareness = asset Hyperawareness Cards.hyperawareness

instance HasAbilities Hyperawareness where
  getAbilities (Hyperawareness a) =
    [ (cardI18n $ withI18nTooltip "hyperawareness.fastSpend1Resource2")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ controlled a 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "hyperawareness.fastSpend1Resource")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #agility)
        $ controlled a 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance RunMessage Hyperawareness where
  runMessage msg a@(Hyperawareness attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 2) iid (SkillModifier #agility 1)
      pure a
    _ -> Hyperawareness <$> liftRunMessage msg attrs
