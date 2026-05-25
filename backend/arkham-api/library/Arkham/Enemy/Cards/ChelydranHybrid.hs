module Arkham.Enemy.Cards.ChelydranHybrid (chelydranHybrid) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Matcher
import Arkham.Trait (Trait (Abomination))

newtype ChelydranHybrid = ChelydranHybrid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chelydranHybrid :: EnemyCard ChelydranHybrid
chelydranHybrid = enemy ChelydranHybrid Cards.chelydranHybrid (2, Static 5, 2) (1, 0)

instance HasModifiersFor ChelydranHybrid where
  getModifiersFor (ChelydranHybrid a) = do
    hasAbomination <- selectAny $ ReadyEnemy <> EnemyWithTrait Abomination <> at_ (locationWithEnemy a)
    isEngaged <- a.id <=~> EnemyIsEngagedWith Anyone
    modifySelfWhen a (hasAbomination || isEngaged) [CannotMove]

instance HasAbilities ChelydranHybrid where
  getAbilities (ChelydranHybrid a) =
    extend1 a
      $ mkAbility a 1
      $ Objective
      $ forced
      $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage ChelydranHybrid where
  runMessage msg e@(ChelydranHybrid attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R3
      pure e
    _ -> ChelydranHybrid <$> liftRunMessage msg attrs
