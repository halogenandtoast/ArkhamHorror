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
      Defeated (EnemyTarget eid') _ _ _ -> eid == eid'
      Do (Defeated (EnemyTarget eid') _ _ _) -> eid == eid'
      After (Defeated (EnemyTarget eid') _ _ _) -> eid == eid'
      CheckWindows ws -> any isEnemyDefeated ws
      Do (CheckWindows ws) -> any isEnemyDefeated ws
      _ -> False
    isEnemyDefeated w = case w.kind of
      Window.EnemyDefeated _ _ eid' -> eid' == eid
      _ -> False
  removeAllMessagesMatchingNested isDiscardEnemy

cancelEnemyDefeatWithWindows :: (ToId enemy EnemyId, HasQueue Message m) => enemy -> m ()
cancelEnemyDefeatWithWindows (asId -> eid) = do
  let
    isDiscardEnemy = \case
      Discard _ _ (EnemyTarget eid') -> eid == eid'
      Discarded (EnemyTarget eid') _ _ -> eid == eid'
      Do (Discarded (EnemyTarget eid') _ _) -> eid == eid'
      RemoveFromPlay (EnemySource eid') -> eid == eid'
      RemovedFromPlay (EnemySource eid') -> eid == eid'
      Defeated (EnemyTarget eid') _ _ _ -> eid == eid'
      Do (Defeated (EnemyTarget eid') _ _ _) -> eid == eid'
      After (Defeated (EnemyTarget eid') _ _ _) -> eid == eid'
      _ -> False
  removeAllMessagesMatchingNested isDiscardEnemy

cancelEnemyDefeatCapture :: (HasQueue Message m, ToId enemy EnemyId) => enemy -> m [Window]
cancelEnemyDefeatCapture (asId -> eid) = do
  -- leave window
  cancelEnemyDefeatWithWindows eid
  -- get after
  after <- fromQueue $ go #after
  -- delete all windows
  removeAllMessagesMatchingNested isDiscardEnemyWindow
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
    case stripQueueWrappers msg of
      CheckWindows ws -> matched timing ws <> go timing msgs
      Do (CheckWindows ws) -> matched timing ws <> go timing msgs
      other -> case queueGroup other of
        Just (children, _) -> go timing children <> go timing msgs
        Nothing -> go timing msgs
  matched timing ws = maybeToList $ find isEnemyDefeated (filter ((== timing) . (.timing)) ws)
