module Arkham.Enemy.Helpers (
  module Arkham.Enemy.Helpers,
  module X,
) where

import Arkham.Classes.HasQueue
import Arkham.Game.Helpers as X
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Target

cancelEnemyDefeat :: HasQueue Message m => EnemyId -> m ()
cancelEnemyDefeat eid = do
  let
    isDiscardEnemy = \case
      Discard _ _ (EnemyTarget eid') -> eid == eid'
      _ -> False
  withQueue_ $ filter (not . isDiscardEnemy)
