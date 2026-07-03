module Arkham.Matcher.OutOfPlayEnemySpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Zone
import TestImport.New

-- Enemy queries default to in-play enemies: an enemy sitting in an OutOfPlay zone
-- (Void/Pursuit/SetAside/Depths/...) is only matched when the query decorates
-- itself with IncludeOutOfPlayEnemy or OutOfPlayEnemy. These specs pin that rule,
-- which the The Secret Name cards below rely on:
--   * The Witch Light   - sets Nahab aside, out of play, then re-finds her.
--   * The Familiar       - spawns the *set-aside* Nahab.
--   * Beyond the Witch House - "find Nahab (even if she is out of play)".
-- Ghostly Presence deliberately says "If Nahab is *in play*", so its bare query
-- is correct as-is and is covered by the in-play case here.
--
-- Nahab's fight modifier reads the current agenda, so (like a real game) each
-- test has an agenda in play.
spec :: Spec
spec = describe "Out-of-play enemy targeting" do
  it "a bare enemy query does NOT match a set-aside (out-of-play) enemy" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    _ <- testAgenda "01105" id
    nahab <- testEnemyWithDef Enemies.nahab id
    run $ PlaceEnemy (toId nahab) (OutOfPlay SetAsideZone)
    assertNone $ enemyIs Enemies.nahab

  it "IncludeOutOfPlayEnemy reaches a set-aside enemy (The Familiar / Beyond the Witch House)"
    . gameTest
    $ \self -> do
      location <- testLocation
      self `moveTo` location
      _ <- testAgenda "01105" id
      nahab <- testEnemyWithDef Enemies.nahab id
      run $ PlaceEnemy (toId nahab) (OutOfPlay SetAsideZone)
      assertAny $ IncludeOutOfPlayEnemy $ enemyIs Enemies.nahab
      selectJust (OutOfPlayEnemy SetAsideZone $ enemyIs Enemies.nahab) `shouldReturn` toId nahab

  it "a bare enemy query still matches an in-play enemy (Ghostly Presence, Site of the Sacrifice)"
    . gameTest
    $ \self -> do
      location <- testLocation
      self `moveTo` location
      _ <- testAgenda "01105" id
      nahab <- testEnemyWithDef Enemies.nahab id
      run $ PlaceEnemy (toId nahab) (AtLocation (toId location))
      assertAny $ enemyIs Enemies.nahab
      -- IncludeOutOfPlayEnemy is a superset, so it still finds the in-play copy.
      assertAny $ IncludeOutOfPlayEnemy $ enemyIs Enemies.nahab

  it "a bare enemy query STILL matches a hidden-in-hand enemy (Brown Jenkin)" . gameTest $ \self -> do
    -- Hidden-in-hand is not an OutOfPlay zone, so the in-play default must not
    -- scope it out -- otherwise "find Brown Jenkin (even if out of play)" and the
    -- forced hand-discard would break.
    location <- testLocation
    self `moveTo` location
    brownJenkin <- testEnemyWithDef Enemies.brownJenkin id
    run $ PlaceEnemy (toId brownJenkin) (HiddenInHand (toId self))
    assertAny $ enemyIs Enemies.brownJenkin

  -- The rule holds for every out-of-play zone, covering cards beyond TSN (Hunting
  -- Horror in the void, pursuit-zone enemies, The Amalgam in the depths, ...).
  for_ [VoidZone, PursuitZone, SetAsideZone, TheDepths] $ \zone ->
    it ("a bare query misses, and OutOfPlayEnemy reaches, an enemy in " <> show zone) . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      _ <- testAgenda "01105" id
      nahab <- testEnemyWithDef Enemies.nahab id
      run $ PlaceEnemy (toId nahab) (OutOfPlay zone)
      assertNone $ enemyIs Enemies.nahab
      selectJust (OutOfPlayEnemy zone $ enemyIs Enemies.nahab) `shouldReturn` toId nahab
