module Arkham.Enemy.Helpers (
  module Arkham.Enemy.Helpers,
  module X,
) where

import Arkham.Classes.HasQueue
import Arkham.Game.Helpers as X
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target

cancelEnemyDefeat :: HasQueue Message m => EnemyId -> m ()
cancelEnemyDefeat eid = do
  let
    isDiscardEnemy = \case
      Discard _ _ (EnemyTarget eid') -> eid == eid'
      Discarded (EnemyTarget eid') _ _ -> eid == eid'
      Do (Discarded (EnemyTarget eid') _ _) -> eid == eid'
      RemoveFromPlay (EnemySource eid') -> eid == eid'
      RemovedFromPlay (EnemySource eid') -> eid == eid'
      EnemyDefeated eid' _ _ _ -> eid == eid'
      After (EnemyDefeated eid' _ _ _) -> eid == eid'
      _ -> False
  withQueue_ $ filter (not . isDiscardEnemy)
