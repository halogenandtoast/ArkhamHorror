module Arkham.Asset.Cards.ThievesKit (thievesKit, ThievesKit (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate

newtype ThievesKit = ThievesKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thievesKit :: AssetCard ThievesKit
thievesKit = asset ThievesKit Cards.thievesKit

instance HasAbilities ThievesKit where
  getAbilities (ThievesKit a) = [restrictedAbility a 1 ControlsThis $ investigateAction $ assetUseCost a Supply 1]

instance RunMessage ThievesKit where
  runMessage msg a@(ThievesKit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigate <- mkInvestigate sid iid (attrs.ability 1)
      chooseOne
        iid
        [ Label "Use {agility}" [toMessage $ withSkillType #agility investigate]
        , Label "Use {intellect}" [toMessage investigate]
        ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      gainResourcesIfCan iid (attrs.ability 1) 1
      pure a
    _ -> ThievesKit <$> liftRunMessage msg attrs
