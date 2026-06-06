module Arkham.Asset.Assets.MiGoWeapon (miGoWeapon) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Token (Token (Ammo))
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype MiGoWeapon = MiGoWeapon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miGoWeapon :: AssetCard MiGoWeapon
miGoWeapon = asset MiGoWeapon Cards.miGoWeapon

instance HasAbilities MiGoWeapon where
  getAbilities (MiGoWeapon a) = [restricted a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage MiGoWeapon where
  runMessage msg a@(MiGoWeapon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      when (n >= 3) do
        withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      withSkillTestTargetedEnemy \enemy -> do
        defeated <- selectAny $ EnemyWithId enemy <> EnemyWithDamage (atLeast 1) <> EnemyWithRemainingHealth (atMost 0)
        unless defeated do
          choices <- select $ connectedFrom (locationWithInvestigator iid) <> LocationCanBeEnteredBy enemy
          chooseOrRunOneM iid $ targets choices (push . EnemyMove enemy)
      pure a
    _ -> MiGoWeapon <$> liftRunMessage msg attrs
