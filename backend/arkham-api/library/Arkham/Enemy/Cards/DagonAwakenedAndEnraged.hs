module Arkham.Enemy.Cards.DagonAwakenedAndEnraged
  ( dagonAwakenedAndEnraged
  , DagonAwakenedAndEnraged(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype DagonAwakenedAndEnraged = DagonAwakenedAndEnraged EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dagonAwakenedAndEnraged :: EnemyCard DagonAwakenedAndEnraged
dagonAwakenedAndEnraged = enemy DagonAwakenedAndEnraged Cards.dagonAwakenedAndEnraged (4, Static 6, 4) (2, 3)

instance RunMessage DagonAwakenedAndEnraged where
  runMessage msg (DagonAwakenedAndEnraged attrs) = runQueueT $ case msg of
    _ -> DagonAwakenedAndEnraged <$> liftRunMessage msg attrs
