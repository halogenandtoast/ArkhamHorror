module Arkham.Asset.Assets.HiredMuscle1 (hiredMuscle1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype HiredMuscle1 = HiredMuscle1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiredMuscle1 :: AssetCard HiredMuscle1
hiredMuscle1 = ally HiredMuscle1 Cards.hiredMuscle1 (3, 1)

instance HasAbilities HiredMuscle1 where
  getAbilities (HiredMuscle1 x) = [restricted x 1 ControlsThis $ forced $ PhaseEnds #when #upkeep]

instance HasModifiersFor HiredMuscle1 where
  getModifiersFor (HiredMuscle1 a) = controllerGets a [SkillModifier #combat 1]

instance RunMessage HiredMuscle1 where
  runMessage msg a@(HiredMuscle1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid do
        labeled "Pay 1 Resource to Hired Muscle" $ spendResources iid 1
        labeled "Discard Hired Muscle" $ toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> HiredMuscle1 <$> liftRunMessage msg attrs
