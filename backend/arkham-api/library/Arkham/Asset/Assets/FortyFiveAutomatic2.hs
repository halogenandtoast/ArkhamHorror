module Arkham.Asset.Assets.FortyFiveAutomatic2 (fortyFiveAutomatic2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype FortyFiveAutomatic2 = FortyFiveAutomatic2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic2 :: AssetCard FortyFiveAutomatic2
fortyFiveAutomatic2 = asset FortyFiveAutomatic2 Cards.fortyFiveAutomatic2

instance HasAbilities FortyFiveAutomatic2 where
  getAbilities (FortyFiveAutomatic2 a) =
    [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveAutomatic2 where
  runMessage msg a@(FortyFiveAutomatic2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [DamageDealt 1, SkillModifier #combat 2, IgnoreRetaliate]
      chooseFightEnemy sid iid attrs
      pure a
    _ -> FortyFiveAutomatic2 <$> liftRunMessage msg attrs
