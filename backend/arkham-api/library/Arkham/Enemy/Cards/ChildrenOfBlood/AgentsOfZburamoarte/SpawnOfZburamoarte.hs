module Arkham.Enemy.Cards.ChildrenOfBlood.AgentsOfZburamoarte.SpawnOfZburamoarte (spawnOfZburamoarte) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SpawnOfZburamoarte = SpawnOfZburamoarte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

spawnOfZburamoarte :: EnemyCard SpawnOfZburamoarte
spawnOfZburamoarte = enemy SpawnOfZburamoarte Cards.spawnOfZburamoarte

instance RunMessage SpawnOfZburamoarte where
  runMessage msg (SpawnOfZburamoarte attrs) = runQueueT $ case msg of
    _ -> SpawnOfZburamoarte <$> liftRunMessage msg attrs
