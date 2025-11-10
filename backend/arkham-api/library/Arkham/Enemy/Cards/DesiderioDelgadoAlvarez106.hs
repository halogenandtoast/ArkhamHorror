module Arkham.Enemy.Cards.DesiderioDelgadoAlvarez106 (desiderioDelgadoAlvarez106) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (insteadOfDamage)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Source (getSourceController)
import Arkham.Matcher

newtype DesiderioDelgadoAlvarez106 = DesiderioDelgadoAlvarez106 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiderioDelgadoAlvarez106 :: EnemyCard DesiderioDelgadoAlvarez106
desiderioDelgadoAlvarez106 = enemy DesiderioDelgadoAlvarez106 Cards.desiderioDelgadoAlvarez106 (4, Static 2, 3) (2, 1)

instance HasAbilities DesiderioDelgadoAlvarez106 where
  getAbilities (DesiderioDelgadoAlvarez106 a) =
    extend
      a
      [ restricted a 1 (oneOf [OnAct 1, OnAct 2])
          $ forced
          $ EnemyTakeDamage #when AnyDamageEffect (be a) (atLeast 1) AnySource
      , restricted a 2 (exists $ enemyIs Cards.desiderioDelgadoAlvarez107 <> at_ YourLocation)
          $ freeReaction
          $ SkillTestResult #after Anyone (WhileEvadingAnEnemy (be a)) (SuccessResult $ atLeast 2)
      ]

instance HasModifiersFor DesiderioDelgadoAlvarez106 where
  getModifiersFor (DesiderioDelgadoAlvarez106 a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance RunMessage DesiderioDelgadoAlvarez106 where
  runMessage msg e@(DesiderioDelgadoAlvarez106 attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      continue iid $ do_ msg
      pure $ DesiderioDelgadoAlvarez106 $ attrs & flippedL .~ True
    Do (LookAtRevealed _iid _ (isTarget attrs -> True)) -> do
      pure $ DesiderioDelgadoAlvarez106 $ attrs & flippedL .~ False
    UseThisAbility iid' (isSource attrs -> True) 1 -> do
      insteadOfDamage attrs \dmg -> do
        iid <- fromMaybe iid' <$> getSourceController dmg.source
        automaticallyEvadeEnemy iid attrs
      pure e
    UseThisAbility _iid' (isSource attrs -> True) 2 -> do
      selectEach (enemyIs Cards.desiderioDelgadoAlvarez107) \desi ->
        nonAttackEnemyDamage Nothing desi 2 attrs
      pure e
    _ -> DesiderioDelgadoAlvarez106 <$> liftRunMessage msg attrs
