module Arkham.Enemy.Helpers where

import Arkham.Classes.HasQueue
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Window qualified as Window

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
      Do (EnemyDefeated eid' _ _ _) -> eid == eid'
      After (EnemyDefeated eid' _ _ _) -> eid == eid'
      CheckWindows ws -> any isEnemyDefeated ws
      Do (CheckWindows ws) -> any isEnemyDefeated ws
      _ -> False
    isEnemyDefeated w = case w.kind of
      Window.EnemyDefeated _ _ eid' -> eid' == eid
      _ -> False
  withQueue_ $ filter (not . isDiscardEnemy)

cancelEnemyDefeatWithWindows :: HasQueue Message m => EnemyId -> m ()
cancelEnemyDefeatWithWindows eid = do
  let
    isDiscardEnemy = \case
      Discard _ _ (EnemyTarget eid') -> eid == eid'
      Discarded (EnemyTarget eid') _ _ -> eid == eid'
      Do (Discarded (EnemyTarget eid') _ _) -> eid == eid'
      RemoveFromPlay (EnemySource eid') -> eid == eid'
      RemovedFromPlay (EnemySource eid') -> eid == eid'
      EnemyDefeated eid' _ _ _ -> eid == eid'
      Do (EnemyDefeated eid' _ _ _) -> eid == eid'
      After (EnemyDefeated eid' _ _ _) -> eid == eid'
      _ -> False
  withQueue_ $ filter (not . isDiscardEnemy)
