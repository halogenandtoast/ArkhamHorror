module Arkham.Behavior.Defeat where

import Arkham.Classes.HasGame (HasGame)
import Arkham.DefeatedBy (DefeatedBy)
import Arkham.Helpers.Window (checkWindows)
import Arkham.Id
import Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

{- | The 'Defeatable' behavior — helpers for the window-firing idioms used by
both 'Arkham.Enemy.Runner' and 'Arkham.EnemyLocation.Runner' when an entity
crosses its damage threshold and is being / has been defeated.

These helpers don't make the defeat decision themselves — that depends on
per-entity rules (swarms, CannotBeDefeated modifiers, etc.). They just bundle
the window-firing for the two events.
-}

-- | Return the @(when, after)@ window-batch messages for
-- @Window.EnemyWouldBeDefeated@. Used to wrap the actual defeat-resolution
-- push so listeners can react before or after.
wouldBeDefeatedWindows
  :: HasGame m
  => EnemyId -> m (Message, Message)
wouldBeDefeatedWindows eid = do
  whenMsg <- checkWindows [mkWhen $ Window.EnemyWouldBeDefeated eid]
  afterMsg <- checkWindows [mkAfter $ Window.EnemyWouldBeDefeated eid]
  pure (whenMsg, afterMsg)

-- | Return the @(when, after)@ window-batch messages for
-- @Window.EnemyDefeated@.
defeatedWindows
  :: HasGame m
  => Maybe InvestigatorId -> DefeatedBy -> EnemyId -> m (Message, Message)
defeatedWindows miid defeatedBy eid = do
  whenMsg <- checkWindows [mkWhen $ Window.EnemyDefeated miid defeatedBy eid]
  afterMsg <- checkWindows [mkAfter $ Window.EnemyDefeated miid defeatedBy eid]
  pure (whenMsg, afterMsg)
