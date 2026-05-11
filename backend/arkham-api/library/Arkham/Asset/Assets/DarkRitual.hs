module Arkham.Asset.Assets.DarkRitual (darkRitual, DarkRitual (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.I18n
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
        $ [Label (withI18n $ countVar 1 $ ikey' "label.spendResources") [SpendResources iid 1] | canSpendResources]
        <> [Label (withI18n $ cardNameVar attrs $ ikey' "label.discardName") [toDiscardBy iid (attrs.ability 1) attrs]]
      pure a
    _ -> DarkRitual <$> runMessage msg attrs
