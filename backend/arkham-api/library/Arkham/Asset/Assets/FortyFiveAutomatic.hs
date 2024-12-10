module Arkham.Asset.Assets.FortyFiveAutomatic (fortyFiveAutomatic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype FortyFiveAutomatic = FortyFiveAutomatic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic :: AssetCard FortyFiveAutomatic
fortyFiveAutomatic = asset FortyFiveAutomatic Cards.fortyFiveAutomatic

instance HasAbilities FortyFiveAutomatic where
  getAbilities (FortyFiveAutomatic a) = [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemyWithModifiers sid iid (attrs.ability 1) [DamageDealt 1, SkillModifier #combat 1]
      pure a
    _ -> FortyFiveAutomatic <$> liftRunMessage msg attrs
