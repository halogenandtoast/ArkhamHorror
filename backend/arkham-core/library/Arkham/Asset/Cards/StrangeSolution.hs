module Arkham.Asset.Cards.StrangeSolution (
  strangeSolution,
  StrangeSolution (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Capability

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetCard StrangeSolution
strangeSolution = asset StrangeSolution Cards.strangeSolution

instance HasAbilities StrangeSolution where
  getAbilities (StrangeSolution x) = [restrictedAbility x 1 ControlsThis actionAbility]

instance RunMessage StrangeSolution where
  runMessage msg a@(StrangeSolution attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (attrs.ability 1) iid #intellect (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let drawing = drawCards iid (attrs.ability 1) 2
      canDraw <- can.draw.cards iid
      pushAll
        $ toDiscardBy iid (attrs.ability 1) attrs
        : [drawing | canDraw]
          <> [Record YouHaveIdentifiedTheSolution]
      pure a
    _ -> StrangeSolution <$> runMessage msg attrs
