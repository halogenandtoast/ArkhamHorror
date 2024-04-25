module Arkham.Asset.Cards.Shotgun4 (Shotgun4 (..), shotgun4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Shotgun4 = Shotgun4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotgun4 :: AssetCard Shotgun4
shotgun4 = asset Shotgun4 Cards.shotgun4

instance HasAbilities Shotgun4 where
  getAbilities (Shotgun4 a) = [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage Shotgun4 where
  runMessage msg a@(Shotgun4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifiers source iid [SkillModifier #combat 3], chooseFight]
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      let val = max 1 (min 5 n)
      push $ skillTestModifier (attrs.ability 1) iid (DamageDealtToInvestigator val)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      push $ skillTestModifiers (attrs.ability 1) iid [NoStandardDamage, DamageDealt $ max 1 (min 5 n)]
      pure a
    _ -> Shotgun4 <$> runMessage msg attrs
