module Arkham.Asset.Cards.SawedOffShotgun5 (SawedOffShotgun5 (..), sawedOffShotgun5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype SawedOffShotgun5 = SawedOffShotgun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sawedOffShotgun5 :: AssetCard SawedOffShotgun5
sawedOffShotgun5 = asset SawedOffShotgun5 Cards.sawedOffShotgun5

instance HasAbilities SawedOffShotgun5 where
  getAbilities (SawedOffShotgun5 a) = [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage SawedOffShotgun5 where
  runMessage msg a@(SawedOffShotgun5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      -- This has to be handled specially for cards like Oops!
      let val = max 1 (min 6 n)
      push $ skillTestModifier (attrs.ability 1) iid (DamageDealtToInvestigator val)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      push $ skillTestModifier (attrs.ability 1) iid (DamageDealt $ max 1 (min 6 n))
      pure a
    _ -> SawedOffShotgun5 <$> runMessage msg attrs
