module Arkham.Asset.Assets.StrangeSolutionEmpoweringElixir4 (strangeSolutionEmpoweringElixir4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StrangeSolutionEmpoweringElixir4 = StrangeSolutionEmpoweringElixir4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionEmpoweringElixir4 :: AssetCard StrangeSolutionEmpoweringElixir4
strangeSolutionEmpoweringElixir4 = asset StrangeSolutionEmpoweringElixir4 Cards.strangeSolutionEmpoweringElixir4

instance HasAbilities StrangeSolutionEmpoweringElixir4 where
  getAbilities (StrangeSolutionEmpoweringElixir4 attrs) =
    [ controlled
        attrs
        1
        (exists $ affectsOthers $ at_ YourLocation <> oneOf [can.gain.resources, can.draw.cards])
        $ actionAbilityWithCost (assetUseCost attrs Supply 1 <> exhaust attrs)
    ]

instance RunMessage StrangeSolutionEmpoweringElixir4 where
  runMessage msg a@(StrangeSolutionEmpoweringElixir4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <-
        select $ affectsOthers $ colocatedWith iid <> oneOf [can.gain.resources, can.draw.cards]
      chooseOrRunOneM iid do
        targets investigators \i -> do
          gainResources i (attrs.ability 1) 2
          drawCards i (attrs.ability 1) 1
      pure a
    _ -> StrangeSolutionEmpoweringElixir4 <$> liftRunMessage msg attrs
