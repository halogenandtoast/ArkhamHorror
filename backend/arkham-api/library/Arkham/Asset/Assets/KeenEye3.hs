module Arkham.Asset.Assets.KeenEye3 (keenEye3, KeenEye3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Modifier

newtype KeenEye3 = KeenEye3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye3 :: AssetCard KeenEye3
keenEye3 = asset KeenEye3 Cards.keenEye3

instance HasAbilities KeenEye3 where
  getAbilities (KeenEye3 a) =
    [ withTooltip "{fast} Spend 2 resources: You get +1 {intellect} until the end of the phase."
        $ restricted a 1 ControlsThis (FastAbility $ ResourceCost 2)
    , withTooltip "{fast} Spend 2 resources: You get +1 {combat} until the end of the phase."
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
