module Arkham.Asset.Assets.FortyFiveThompson (fortyFiveThompson) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Modifier

newtype FortyFiveThompson = FortyFiveThompson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompson :: AssetCard FortyFiveThompson
fortyFiveThompson = asset FortyFiveThompson Cards.fortyFiveThompson

instance HasAbilities FortyFiveThompson where
  getAbilities (FortyFiveThompson a) = [controlled_ a 1 $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveThompson where
  runMessage msg a@(FortyFiveThompson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2]
      chooseFightEnemy sid iid source
      pure a
    _ -> FortyFiveThompson <$> liftRunMessage msg attrs
