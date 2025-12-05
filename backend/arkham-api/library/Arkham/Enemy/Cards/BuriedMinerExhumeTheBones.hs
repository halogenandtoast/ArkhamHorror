module Arkham.Enemy.Cards.BuriedMinerExhumeTheBones (buriedMinerExhumeTheBones) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Story.Cards qualified as Stories

newtype BuriedMinerExhumeTheBones = BuriedMinerExhumeTheBones EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

buriedMinerExhumeTheBones :: EnemyCard BuriedMinerExhumeTheBones
buriedMinerExhumeTheBones = enemy BuriedMinerExhumeTheBones Cards.buriedMinerExhumeTheBones (0, Static 1, 0) (0, 0)

instance RunMessage BuriedMinerExhumeTheBones where
  runMessage msg e@(BuriedMinerExhumeTheBones attrs) = runQueueT $ case msg of
    LookAtRevealed iid _ (isTarget attrs -> True) -> do
      let exhumeTheBones = lookupCard Stories.exhumeTheBones (toCardId attrs)
      focusCards [exhumeTheBones] $ continue_ iid
      pure e
    _ -> BuriedMinerExhumeTheBones <$> liftRunMessage msg attrs
