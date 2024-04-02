module Arkham.Asset.Cards.TwentyFiveAutomatic (twentyFiveAutomatic, TwentyFiveAutomatic (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype TwentyFiveAutomatic = TwentyFiveAutomatic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twentyFiveAutomatic :: AssetCard TwentyFiveAutomatic
twentyFiveAutomatic = asset TwentyFiveAutomatic Cards.twentyFiveAutomatic

instance HasAbilities TwentyFiveAutomatic where
  getAbilities (TwentyFiveAutomatic attrs) = [restrictedAbility attrs 1 ControlsThis $ fightAction $ assetUseCost attrs Ammo 1]

instance RunMessage TwentyFiveAutomatic where
  runMessage msg a@(TwentyFiveAutomatic attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    ChoseEnemy iid (isAbilitySource attrs 1 -> True) eid -> do
      exhausted <- eid <=~> ExhaustedEnemy
      when exhausted do
        push $ skillTestModifiers (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      pure a
    _ -> TwentyFiveAutomatic <$> runMessage msg attrs
