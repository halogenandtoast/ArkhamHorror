module Arkham.Enemy.Helpers where

import Arkham.Classes.HasQueue
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window)
import Arkham.Window qualified as Window

cancelEnemyDefeat :: (HasQueue Message m, ToId enemy EnemyId) => enemy -> m ()
cancelEnemyDefeat (asId -> eid) = do
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

cancelEnemyDefeatWithWindows :: (ToId enemy EnemyId, HasQueue Message m) => enemy -> m ()
cancelEnemyDefeatWithWindows (asId -> eid) = do
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

cancelEnemyDefeatCapture :: (HasQueue Message m, ToId enemy EnemyId) => enemy -> m [Window]
cancelEnemyDefeatCapture (asId -> eid) = do
  -- leave window
  cancelEnemyDefeatWithWindows eid
  -- get after
  after <- fromQueue $ go #after
  -- delete all windows
  withQueue_ $ filter (not . isDiscardEnemyWindow)
  pure after
 where
  isDiscardEnemyWindow = \case
    CheckWindows ws -> any isEnemyDefeated ws
    Do (CheckWindows ws) -> any isEnemyDefeated ws
    _ -> False
  isEnemyDefeated w = case w.kind of
    Window.EnemyDefeated _ _ eid' -> eid' == eid
    _ -> False
  go _timing [] = []
  go timing (msg : msgs) = do
    case msg of
      CheckWindows ws -> case find isEnemyDefeated (filter ((== timing) . (.timing)) ws) of
        Just w -> w : go timing msgs
        Nothing -> go timing msgs
      Do (CheckWindows ws) -> case find isEnemyDefeated (filter ((== timing) . (.timing)) ws) of
        Just w -> w : go timing msgs
        Nothing -> go timing msgs
      _ -> go timing msgs
