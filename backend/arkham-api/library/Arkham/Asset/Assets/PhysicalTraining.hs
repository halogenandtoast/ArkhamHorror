module Arkham.Asset.Assets.PhysicalTraining (physicalTraining) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype PhysicalTraining = PhysicalTraining AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalTraining :: AssetCard PhysicalTraining
physicalTraining = asset PhysicalTraining Cards.physicalTraining

instance HasAbilities PhysicalTraining where
  getAbilities (PhysicalTraining a) =
    [ (cardI18n $ withI18nTooltip "physicalTraining.fastSpend1Resource2")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #willpower)
        $ controlled a 1 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    , (cardI18n $ withI18nTooltip "physicalTraining.fastSpend1Resource")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ controlled a 2 DuringAnySkillTest (FastAbility $ ResourceCost 1)
    ]

instance RunMessage PhysicalTraining where
  runMessage msg a@(PhysicalTraining attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      modifySkillTest (attrs.ability 1) iid [SkillModifier #willpower 1]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      modifySkillTest (attrs.ability 2) iid [SkillModifier #combat 1]
      pure a
    _ -> PhysicalTraining <$> liftRunMessage msg attrs
