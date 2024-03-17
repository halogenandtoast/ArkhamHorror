module Arkham.Enemy.Cards.LumberingGug (lumberingGug, LumberingGug (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude

newtype LumberingGug = LumberingGug EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lumberingGug :: EnemyCard LumberingGug
lumberingGug = enemy LumberingGug Cards.lumberingGug (4, Static 6, 2) (2, 2)

instance HasAbilities LumberingGug where
  getAbilities (LumberingGug attrs) =
    extend attrs [mkAbility attrs 1 $ forced $ EnemyExhausts #after (be attrs)]

instance RunMessage LumberingGug where
  runMessage msg e@(LumberingGug attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ nextPhaseModifier UpkeepPhase (attrs.ability 1) attrs DoesNotReadyDuringUpkeep
      pure e
    _ -> LumberingGug <$> runMessage msg attrs
