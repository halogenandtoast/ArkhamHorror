module Arkham.Asset.Assets.KeenEye (keenEye, KeenEye (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Modifier

newtype KeenEye = KeenEye AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye :: AssetCard KeenEye
keenEye = asset KeenEye Cards.keenEye

instance HasAbilities KeenEye where
  getAbilities (KeenEye a) =
    [ (cardI18n $ withI18nTooltip "keenEye.fastSpend2Resources2")
        $ restrictedAbility a 1 ControlsThis (FastAbility $ ResourceCost 2)
    , (cardI18n $ withI18nTooltip "keenEye.fastSpend2Resources")
        $ restrictedAbility a 2 ControlsThis (FastAbility $ ResourceCost 2)
    ]

instance RunMessage KeenEye where
  runMessage msg a@(KeenEye attrs) = runQueueT $ case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      phaseModifier source iid $ SkillModifier #intellect 1
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      phaseModifier source iid $ SkillModifier #combat 1
      pure a
    _ -> KeenEye <$> liftRunMessage msg attrs
