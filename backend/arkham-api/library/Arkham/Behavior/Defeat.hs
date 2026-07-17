module Arkham.Behavior.Defeat where

import Arkham.Classes.HasGame (HasGame)
import Arkham.DefeatedBy (DefeatedBy (..))
import Arkham.Helpers.Window (checkWindows)
import Arkham.Id
import Arkham.Message (Message (After, Do, When))
import Arkham.Prelude
import Arkham.Source (Source)
import Arkham.Window (mkAfter, mkWhen)
import Arkham.Window qualified as Window

{- | The 'Defeatable' behavior — the defeat pipeline shared by 'Arkham.Enemy.Runner'
and 'Arkham.EnemyLocation.Runner'.

An enemy-location is an enemy, so it must defeat exactly like one: same
classification, same windows, same order. Both runners therefore build their
@Defeated@ handling out of 'classifyDefeat', 'openDefeat' and 'closeDefeat'
rather than firing windows themselves — a window added here reaches both by
construction. Enemy-locations previously hand-rolled their own smaller version
of this and silently drifted (no 'Window.IfEnemyDefeated', @defeatedBy@ stuck on
'DefeatedByOther'); see issue #5162.

These helpers don't make the defeat decision itself — that depends on per-entity
rules (swarms, CannotBeDefeated modifiers, etc.) and stays in the runners.
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

-- | Classify a defeat by whether the entity's damage reached its modified health.
-- Reactions restricted to @ByDamage@ key off this.
classifyDefeat :: Source -> Int -> Maybe Int -> DefeatedBy
classifyDefeat source damage mHealth
  | maybe False (damage >=) mHealth = DefeatedByDamage source
  | otherwise = DefeatedByOther source

-- | The messages to push for @Defeated@: open the defeat, then schedule the
-- @Do@ (disposal) and @After@ (mark defeated) halves.
--
-- @extra@ is entity-specific work that resolves inside the defeat itself, before
-- disposal — an enemy handing off its keys, for instance.
openDefeat
  :: HasGame m
  => EnemyId -> DefeatedBy -> Maybe InvestigatorId -> Message -> [Message] -> m [Message]
openDefeat eid defeatedBy miid msg extra = do
  whenMsg <- checkWindows [mkWhen $ Window.EnemyDefeated miid defeatedBy eid]
  pure $ [whenMsg, When msg] <> extra <> [Do msg, After msg]

-- | The messages to push for @Do (Defeated ...)@: the after-windows bracketing
-- @disposal@, which is how the entity itself leaves play (discard or victory for
-- an enemy; nothing for an enemy-location, whose removal is the scenario's job
-- and rides on @After@ instead).
--
-- 'Window.IfEnemyDefeated' deliberately resolves after disposal, so reactions
-- that read the entity back do so through 'Arkham.Matcher.DefeatedEnemy'.
closeDefeat
  :: HasGame m
  => EnemyId -> DefeatedBy -> Maybe InvestigatorId -> [Message] -> m [Message]
closeDefeat eid defeatedBy miid disposal = do
  afterDefeatMsg <- checkWindows [mkAfter $ Window.EnemyDefeated miid defeatedBy eid]
  ifDefeatedMsg <- checkWindows [mkAfter $ Window.IfEnemyDefeated miid defeatedBy eid]
  pure $ [afterDefeatMsg] <> disposal <> [ifDefeatedMsg]
