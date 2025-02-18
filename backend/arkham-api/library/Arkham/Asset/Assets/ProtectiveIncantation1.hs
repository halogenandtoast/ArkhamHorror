module Arkham.Asset.Assets.ProtectiveIncantation1 (protectiveIncantation1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Cost
import Arkham.Matcher
import Arkham.Prelude

newtype ProtectiveIncantation1 = ProtectiveIncantation1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectiveIncantation1 :: AssetCard ProtectiveIncantation1
protectiveIncantation1 =
  asset ProtectiveIncantation1 Cards.protectiveIncantation1

instance HasAbilities ProtectiveIncantation1 where
  getAbilities (ProtectiveIncantation1 a) =
    [restricted a 1 ControlsThis $ forced $ TurnEnds #when $ HasMatchingAsset (be a)]

instance RunMessage ProtectiveIncantation1 where
  runMessage msg a@(ProtectiveIncantation1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      hasResources <- (> 0) <$> getSpendableResources iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label "Discard Protective Incantation" [toDiscardBy iid (toAbilitySource attrs 1) attrs]
        : [Label "Spend 1 resource" [SpendResources iid 1] | hasResources]
      pure a
    _ -> ProtectiveIncantation1 <$> runMessage msg attrs
