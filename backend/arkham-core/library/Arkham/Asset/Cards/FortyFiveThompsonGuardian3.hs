module Arkham.Asset.Cards.FortyFiveThompsonGuardian3 (
  fortyFiveThompsonGuardian3,
  FortyFiveThompsonGuardian3 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype FortyFiveThompsonGuardian3 = FortyFiveThompsonGuardian3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompsonGuardian3 :: AssetCard FortyFiveThompsonGuardian3
fortyFiveThompsonGuardian3 = asset FortyFiveThompsonGuardian3 Cards.fortyFiveThompsonGuardian3

instance HasAbilities FortyFiveThompsonGuardian3 where
  getAbilities (FortyFiveThompsonGuardian3 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveThompsonGuardian3 where
  runMessage msg a@(FortyFiveThompsonGuardian3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2], chooseFight]
      pure a
    SpendUses _ (isTarget attrs -> True) Ammo n -> do
      for_ attrs.controller \iid -> do
        push $ PlaceResources (toSource attrs) (toTarget iid) n
      FortyFiveThompsonGuardian3 <$> runMessage msg attrs
    _ -> FortyFiveThompsonGuardian3 <$> runMessage msg attrs
