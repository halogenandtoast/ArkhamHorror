module Arkham.Asset.Cards.DarkRitual (darkRitual, DarkRitual (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Matcher
import Arkham.Prelude

newtype DarkRitual = DarkRitual AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkRitual :: AssetCard DarkRitual
darkRitual = asset DarkRitual Cards.darkRitual

instance HasAbilities DarkRitual where
  getAbilities (DarkRitual attrs) = [restrictedAbility attrs 1 ControlsThis $ forced $ PhaseEnds #when #mythos]

instance RunMessage DarkRitual where
  runMessage msg a@(DarkRitual attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canSpendResources <- can.spend.resources iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
        $ [Label "Spend 1 resource" [SpendResources iid 1] | canSpendResources]
        <> [Label "Discard Dark Ritual" [toDiscardBy iid (attrs.ability 1) attrs]]
      pure a
    _ -> DarkRitual <$> runMessage msg attrs
