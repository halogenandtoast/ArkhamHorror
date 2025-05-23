module Arkham.Asset.Assets.Shotgun4 (shotgun4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype Shotgun4 = Shotgun4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotgun4 :: AssetCard Shotgun4
shotgun4 = asset Shotgun4 Cards.shotgun4

instance HasAbilities Shotgun4 where
  getAbilities (Shotgun4 a) = [fightAbility a 1 (assetUseCost a #ammo 1) ControlsThis]

instance RunMessage Shotgun4 where
  runMessage msg a@(Shotgun4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [NoStandardDamage, SkillModifier #combat 3]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      withSkillTest \sid -> do
        let val = max 1 (min 5 n)
        -- sort of annoying but we need to handle oops here, but also the investigator damage
        skillTestModifiers sid (attrs.ability 1) iid [DamageDealt 1, DamageDealtToInvestigator (val - 1)]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt $ max 1 (min 5 n))
      pure a
    _ -> Shotgun4 <$> liftRunMessage msg attrs
