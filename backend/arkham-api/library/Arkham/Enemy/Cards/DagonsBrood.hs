module Arkham.Enemy.Cards.DagonsBrood
  ( dagonsBrood
  , DagonsBrood(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DagonsBrood = DagonsBrood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dagonsBrood :: EnemyCard DagonsBrood
dagonsBrood = enemy DagonsBrood Cards.dagonsBrood (0, Static 1, 0) (0, 0)

instance RunMessage DagonsBrood where
  runMessage msg (DagonsBrood attrs) = runQueueT $ case msg of
    _ -> DagonsBrood <$> liftRunMessage msg attrs
