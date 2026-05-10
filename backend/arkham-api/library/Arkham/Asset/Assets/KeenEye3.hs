module Arkham.Asset.Assets.KeenEye3 (keenEye3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher.SkillTest
import Arkham.Modifier

newtype KeenEye3 = KeenEye3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye3 :: AssetCard KeenEye3
keenEye3 = asset KeenEye3 Cards.keenEye3

instance HasAbilities KeenEye3 where
  getAbilities (KeenEye3 a) =
    [ (cardI18n $ withI18nTooltip "keenEye3.fastSpend2Resources2")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #intellect)
        $ restricted a 1 ControlsThis (FastAbility $ ResourceCost 2)
    , (cardI18n $ withI18nTooltip "keenEye3.fastSpend2Resources")
        $ wantsSkillTest (YourSkillTest $ SkillTestWants #combat)
        $ restricted a 2 ControlsThis (FastAbility $ ResourceCost 2)
    ]

instance RunMessage KeenEye3 where
  runMessage msg a@(KeenEye3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      phaseModifier attrs iid (SkillModifier #intellect 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      phaseModifier attrs iid (SkillModifier #combat 1)
      pure a
    _ -> KeenEye3 <$> liftRunMessage msg attrs
