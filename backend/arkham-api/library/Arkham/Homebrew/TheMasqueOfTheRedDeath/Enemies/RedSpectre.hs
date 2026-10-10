module Arkham.Homebrew.TheMasqueOfTheRedDeath.Enemies.RedSpectre (redSpectre) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (describedSkullEffect)
import Arkham.Matcher

newtype RedSpectre = RedSpectre EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redSpectre :: EnemyCard RedSpectre
redSpectre = enemy RedSpectre Cards.redSpectre

instance HasAbilities RedSpectre where
  -- "Red Spectre's location gains: '[skull]: If this test fails and Red Spectre
  -- is ready, it makes an immediate attack against you.'"
  getAbilities (RedSpectre a) =
    extend1 a
      $ describedSkullEffect
        0
        "If this test fails and Red Spectre is ready, it makes an immediate attack against you."
        (proxied (locationWithEnemy a.id) a)
        1

instance RunMessage RedSpectre where
  runMessage msg e@(RedSpectre attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      withSkillTest \sid ->
        -- readiness is re-read when the rider resolves, not when the token was revealed
        onFailedByEffect sid (atLeast 0) (attrs.ability 1) iid
          $ whenM (selectAny $ be attrs <> ReadyEnemy)
          $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> RedSpectre <$> liftRunMessage msg attrs
