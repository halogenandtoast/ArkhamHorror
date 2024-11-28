module Arkham.Enemy.Cards.RoaldEllsworthIntrepidExplorer
  ( roaldEllsworthIntrepidExplorer
  , RoaldEllsworthIntrepidExplorer(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype RoaldEllsworthIntrepidExplorer = RoaldEllsworthIntrepidExplorer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

roaldEllsworthIntrepidExplorer :: EnemyCard RoaldEllsworthIntrepidExplorer
roaldEllsworthIntrepidExplorer = enemy RoaldEllsworthIntrepidExplorer Cards.roaldEllsworthIntrepidExplorer (0, Static 1, 0) (0, 0)

instance RunMessage RoaldEllsworthIntrepidExplorer where
  runMessage msg (RoaldEllsworthIntrepidExplorer attrs) = runQueueT $ case msg of
    _ -> RoaldEllsworthIntrepidExplorer <$> liftRunMessage msg attrs
