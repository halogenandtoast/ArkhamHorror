module Arkham.Asset.Cards.FortyFiveThompson (fortyFiveThompson, FortyFiveThompson (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype FortyFiveThompson = FortyFiveThompson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompson :: AssetCard FortyFiveThompson
fortyFiveThompson = asset FortyFiveThompson Cards.fortyFiveThompson

instance HasAbilities FortyFiveThompson where
  getAbilities (FortyFiveThompson a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveThompson where
  runMessage msg a@(FortyFiveThompson attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [DamageDealt 1, SkillModifier #combat 2], chooseFight]
      pure a
    _ -> FortyFiveThompson <$> runMessage msg attrs
