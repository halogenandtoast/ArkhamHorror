module Arkham.Enemy.Cards.DesiderioDelgadoAlvarez107 (desiderioDelgadoAlvarez107) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (insteadOfDamage)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Source (getSourceController)
import Arkham.Matcher

newtype DesiderioDelgadoAlvarez107 = DesiderioDelgadoAlvarez107 EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiderioDelgadoAlvarez107 :: EnemyCard DesiderioDelgadoAlvarez107
desiderioDelgadoAlvarez107 = enemy DesiderioDelgadoAlvarez107 Cards.desiderioDelgadoAlvarez107 (4, Static 2, 3) (2, 1)

instance HasAbilities DesiderioDelgadoAlvarez107 where
  getAbilities (DesiderioDelgadoAlvarez107 a) =
    extend
      a
      [ restricted a 1 (oneOf [OnAct 1, OnAct 2])
          $ forced
          $ EnemyTakeDamage #when AnyDamageEffect (be a) (atLeast 1) AnySource
      , restricted a 2 (exists $ enemyIs Cards.desiderioDelgadoAlvarez106 <> at_ YourLocation)
          $ freeReaction
          $ SkillTestResult #after Anyone (WhileEvadingAnEnemy (be a)) (SuccessResult $ atLeast 2)
      ]

instance HasModifiersFor DesiderioDelgadoAlvarez107 where
  getModifiersFor (DesiderioDelgadoAlvarez107 a) = do
    n <- perPlayer 2
    modifySelf a [HealthModifier n]

instance RunMessage DesiderioDelgadoAlvarez107 where
  runMessage msg e@(DesiderioDelgadoAlvarez107 attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      continue iid $ do_ msg
      pure $ DesiderioDelgadoAlvarez107 $ attrs & flippedL .~ True
    Do (LookAtRevealed _iid _ (isTarget attrs -> True)) -> do
      pure $ DesiderioDelgadoAlvarez107 $ attrs & flippedL .~ False
    UseThisAbility iid' (isSource attrs -> True) 1 -> do
      insteadOfDamage attrs \dmg -> do
        iid <- fromMaybe iid' <$> getSourceController dmg.source
        automaticallyEvadeEnemy iid attrs
      pure e
    UseThisAbility _iid' (isSource attrs -> True) 2 -> do
      selectEach (enemyIs Cards.desiderioDelgadoAlvarez106) \desi ->
        nonAttackEnemyDamage Nothing desi 2 attrs
      pure e
    _ -> DesiderioDelgadoAlvarez107 <$> liftRunMessage msg attrs
