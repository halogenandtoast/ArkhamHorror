module Arkham.Enemy.Cards.KhalidBelovedCompanion (khalidBelovedCompanion) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype KhalidBelovedCompanion = KhalidBelovedCompanion EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

khalidBelovedCompanion :: EnemyCard KhalidBelovedCompanion
khalidBelovedCompanion = enemy KhalidBelovedCompanion Cards.khalidBelovedCompanion (4, Static 5, 3) (2, 1)

instance RunMessage KhalidBelovedCompanion where
  runMessage msg (KhalidBelovedCompanion attrs) = runQueueT $ case msg of
    _ -> KhalidBelovedCompanion <$> liftRunMessage msg attrs
