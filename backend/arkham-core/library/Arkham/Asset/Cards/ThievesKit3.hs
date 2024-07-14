module Arkham.Asset.Cards.ThievesKit3 (thievesKit3, ThievesKit3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Investigate
import Arkham.Modifier

newtype ThievesKit3 = ThievesKit3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thievesKit3 :: AssetCard ThievesKit3
thievesKit3 = asset ThievesKit3 Cards.thievesKit3

instance HasAbilities ThievesKit3 where
  getAbilities (ThievesKit3 a) = [restrictedAbility a 1 ControlsThis $ investigateAction $ assetUseCost a Supply 1]

instance RunMessage ThievesKit3 where
  runMessage msg a@(ThievesKit3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigate <- mkInvestigate iid (attrs.ability 1)
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      chooseOne
        iid
        [ Label "Use {agility}" [toMessage $ withSkillType #agility investigate]
        , Label "Use {intellect}" [toMessage investigate]
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      gainResourcesIfCan iid (attrs.ability 1) (if n >= 2 then 2 else 1)
      pure a
    _ -> ThievesKit3 <$> liftRunMessage msg attrs
