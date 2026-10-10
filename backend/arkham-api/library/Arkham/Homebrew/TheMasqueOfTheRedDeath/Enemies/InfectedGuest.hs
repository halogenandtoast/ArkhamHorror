module Arkham.Homebrew.TheMasqueOfTheRedDeath.Enemies.InfectedGuest (infectedGuest) where

import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (describedSkullEffect)
import Arkham.Matcher
import Arkham.Modifier

newtype InfectedGuest = InfectedGuest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infectedGuest :: EnemyCard InfectedGuest
infectedGuest = enemy InfectedGuest Cards.infectedGuest

instance HasAbilities InfectedGuest where
  -- "Infected Guest's location gains: '[skull]: -1. Deal 1 damage to Infected Guest.'"
  getAbilities (InfectedGuest a) =
    extend1 a
      $ describedSkullEffect (-1) "Deal 1 damage to Infected Guest." (proxied (locationWithEnemy a.id) a) 1

instance RunMessage InfectedGuest where
  runMessage msg e@(InfectedGuest attrs) = runQueueT $ case msg of
    UseThisAbility _ (isProxySource attrs -> True) 1 -> do
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 1) sid
          $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 1))
      nonAttackEnemyDamage Nothing (attrs.ability 1) 1 attrs.id
      pure e
    _ -> InfectedGuest <$> liftRunMessage msg attrs
