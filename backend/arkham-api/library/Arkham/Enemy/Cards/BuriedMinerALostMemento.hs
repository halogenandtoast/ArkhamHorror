module Arkham.Enemy.Cards.BuriedMinerALostMemento (buriedMinerALostMemento) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype BuriedMinerALostMemento = BuriedMinerALostMemento EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

buriedMinerALostMemento :: EnemyCard BuriedMinerALostMemento
buriedMinerALostMemento = enemy BuriedMinerALostMemento Cards.buriedMinerALostMemento (0, Static 1, 0) (0, 0)

instance RunMessage BuriedMinerALostMemento where
  runMessage msg e@(BuriedMinerALostMemento attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let aLostMemento = lookupCard Stories.aLostMemento (toCardId attrs)
      focusCards [aLostMemento] $ continue_ iid
      pure e
    _ -> BuriedMinerALostMemento <$> liftRunMessage msg attrs
