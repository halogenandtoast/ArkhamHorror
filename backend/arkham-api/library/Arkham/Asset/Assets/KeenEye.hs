module Arkham.Asset.Assets.KeenEye (keenEye, KeenEye (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype KeenEye = KeenEye AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye :: AssetCard KeenEye
keenEye = asset KeenEye Cards.keenEye

instance HasAbilities KeenEye where
  getAbilities (KeenEye a) =
    [ withTooltip
        "{fast} Spend 2 resources: You get +1 {intellect} until the end of the phase"
        $ restrictedAbility a 1 ControlsThis (FastAbility $ ResourceCost 2)
    , withTooltip
        "{fast} Spend 2 resources: You get +1 {combat} until the end of the phase"
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
