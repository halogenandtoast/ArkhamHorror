module Arkham.Asset.Assets.Crowbar (crowbar) where

import Arkham.Ability
import Arkham.Aspect.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Investigate
import Arkham.Modifier

newtype Crowbar = Crowbar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crowbar :: AssetCard Crowbar
crowbar = asset Crowbar Cards.crowbar

instance HasAbilities Crowbar where
  getAbilities (Crowbar a) =
    [ investigateAbility a 1 (exhaust a) ControlsThis
    , fightAbility a 2 (exhaust a) ControlsThis
    ]

instance RunMessage Crowbar where
  runMessage msg a@(Crowbar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let source = attrs.ability 1
      aspect iid source (#combat `InsteadOf` #intellect) (mkInvestigate sid iid source)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    _ -> Crowbar <$> liftRunMessage msg attrs
