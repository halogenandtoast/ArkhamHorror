module Arkham.Asset.Assets.StrangeSolution (strangeSolution) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Message.Lifted.Log

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetCard StrangeSolution
strangeSolution = asset StrangeSolution Cards.strangeSolution

instance HasAbilities StrangeSolution where
  getAbilities (StrangeSolution x) = [skillTestAbility $ restricted x 1 ControlsThis actionAbility]

instance RunMessage StrangeSolution where
  runMessage msg a@(StrangeSolution attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      drawCards iid (attrs.ability 1) 2
      record YouHaveIdentifiedTheSolution
      pure a
    _ -> StrangeSolution <$> liftRunMessage msg attrs
