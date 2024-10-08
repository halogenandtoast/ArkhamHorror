module Arkham.Enemy.Cards.HybridAssassin
  ( hybridAssassin
  , HybridAssassin(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HybridAssassin = HybridAssassin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hybridAssassin :: EnemyCard HybridAssassin
hybridAssassin = enemy HybridAssassin Cards.hybridAssassin (3, Static 3, 1) (0, 1)

instance RunMessage HybridAssassin where
  runMessage msg (HybridAssassin attrs) = runQueueT $ case msg of
    _ -> HybridAssassin <$> liftRunMessage msg attrs
