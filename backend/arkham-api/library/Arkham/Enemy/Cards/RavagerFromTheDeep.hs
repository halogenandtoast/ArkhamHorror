module Arkham.Enemy.Cards.RavagerFromTheDeep
  ( ravagerFromTheDeep
  , RavagerFromTheDeep(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype RavagerFromTheDeep = RavagerFromTheDeep EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ravagerFromTheDeep :: EnemyCard RavagerFromTheDeep
ravagerFromTheDeep = enemy RavagerFromTheDeep Cards.ravagerFromTheDeep (2, Static 4, 1) (2, 1)

instance RunMessage RavagerFromTheDeep where
  runMessage msg (RavagerFromTheDeep attrs) = runQueueT $ case msg of
    _ -> RavagerFromTheDeep <$> liftRunMessage msg attrs
