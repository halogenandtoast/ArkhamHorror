module Arkham.Enemy.Cards.Golem (golem) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers

newtype Golem = Golem EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

golem :: EnemyCard Golem
golem = enemy Golem Cards.golem (1, Static 1, 1) (1, 0)

instance RunMessage Golem where
  runMessage msg e@(Golem attrs) = runQueueT $ case msg of 
    Do (EnemyDefeated eid _ _ _) | eid == attrs.id -> do
      removeFromGame attrs
      case toCard attrs of
        c@(PlayerCard pc) -> for_ pc.owner (`hollow` c)
        _ -> pure ()
      pure e
    _ -> Golem <$> liftRunMessage msg attrs
