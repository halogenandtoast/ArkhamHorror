module Arkham.Enemy.Cards.VulnerableHeart (vulnerableHeart) where

import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Modifiers (
  ModifierType (CannotBeDefeated, CannotBeEvaded, CannotMakeAttacksOfOpportunity, EnemyFight),
  modifySelf,
 )
import Arkham.Projection
import Arkham.Token qualified as Token

newtype VulnerableHeart = VulnerableHeart EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor VulnerableHeart where
  getModifiersFor (VulnerableHeart a) = do
    dmg <- field EnemyDamage a.id
    modifySelf a [CannotBeDefeated, CannotBeEvaded, CannotMakeAttacksOfOpportunity, EnemyFight dmg]

vulnerableHeart :: EnemyCard VulnerableHeart
vulnerableHeart = enemyWith VulnerableHeart Cards.vulnerableHeart $ \a ->
  a {enemyEvade = Nothing}

instance RunMessage VulnerableHeart where
  runMessage msg e@(VulnerableHeart attrs) = runQueueT $ case msg of
    Damaged (EnemyTarget eid) damageAssignment | eid == attrs.id -> do
      let remainingDamage = max 0 (15 - attrs.damage)
      if remainingDamage <= 0
        then pure e
        else
          VulnerableHeart
            <$> liftRunMessage
              ( Damaged
                  (EnemyTarget eid)
                  damageAssignment {damageAssignmentAmount = min remainingDamage damageAssignment.amount}
              )
              attrs
    Do (PlaceTokens source (EnemyTarget eid) Token.Damage n) | eid == attrs.id -> do
      let amount = min n $ max 0 (15 - attrs.damage)
      if amount <= 0
        then pure e
        else
          VulnerableHeart
            <$> liftRunMessage (Do $ PlaceTokens source (EnemyTarget eid) Token.Damage amount) attrs
    _ -> VulnerableHeart <$> liftRunMessage msg attrs
