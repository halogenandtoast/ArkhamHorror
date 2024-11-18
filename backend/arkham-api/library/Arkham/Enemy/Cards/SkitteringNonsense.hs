module Arkham.Enemy.Cards.SkitteringNonsense (skitteringNonsense, SkitteringNonsense (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SkitteringNonsense = SkitteringNonsense EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

skitteringNonsense :: EnemyCard SkitteringNonsense
skitteringNonsense = enemy SkitteringNonsense Cards.skitteringNonsense (2, Static 2, 4) (1, 1)

instance RunMessage SkitteringNonsense where
  runMessage msg (SkitteringNonsense attrs) = runQueueT $ case msg of
    _ -> SkitteringNonsense <$> liftRunMessage msg attrs
