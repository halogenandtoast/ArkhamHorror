module Arkham.Enemy.Cards.LumberingGug (lumberingGug, LumberingGug (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Phase

newtype LumberingGug = LumberingGug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lumberingGug :: EnemyCard LumberingGug
lumberingGug = enemy LumberingGug Cards.lumberingGug (4, Static 6, 2) (2, 2)

instance HasAbilities LumberingGug where
  getAbilities (LumberingGug attrs) =
    extend1 attrs $ mkAbility attrs 1 $ forced $ EnemyExhausts #after (be attrs)

instance RunMessage LumberingGug where
  runMessage msg e@(LumberingGug attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      nextPhaseModifier UpkeepPhase (attrs.ability 1) attrs DoesNotReadyDuringUpkeep
      pure e
    _ -> LumberingGug <$> liftRunMessage msg attrs
