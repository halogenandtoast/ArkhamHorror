module Arkham.Event.Events.Doppelganger1Spec (spec) where

import Arkham.Calculation
import Arkham.Enemy.Types qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Doppelgänger (1)" $ do
  -- Regression for #4759: an enemy that enters your location (here, by spawning
  -- engaged at it) must engage you BEFORE Doppelgänger's "after an enemy enters
  -- attached location" reaction window opens. Previously the reaction window
  -- fired while the enemy was still unengaged, because engagement was queued
  -- after the after-enters window. We assert engagement state at the pending
  -- reaction window: pre-fix it was [] here, post-fix it is the entering enemy.
  it "engages the entering enemy before its reaction window (#4759)" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    enemy <- testEnemyWith (EnemyAttrs.evadeL ?~ Fixed 3)
    playEvent self Events.doppelganger1
    spawnAt enemy location
    self.engagedEnemies `shouldReturn` [toId enemy]
