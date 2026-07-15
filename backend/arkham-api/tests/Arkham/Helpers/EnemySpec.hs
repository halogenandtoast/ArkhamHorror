module Arkham.Helpers.EnemySpec (spec) where

import Arkham.Enemy.Types (Field (EnemyHealthActual))
import Arkham.Helpers.Enemy (getDefeatedEnemyHealth)
import Arkham.Helpers.Enemy qualified as Helpers
import TestImport.New

spec :: Spec
spec = describe "getDefeatedEnemyHealth" do
  -- A removed enemy keeps its entity (placed OutOfPlay RemovedZone), but
  -- clearRemovedEntities drops it from the entity map once the ability that
  -- defeated it resolves, and again at BeginTurn. An IfEnemyDefeated window
  -- answered after that finds no enemy at all, so the health has to come from
  -- the record made when it was defeated, or cards reading it silently do
  -- nothing (Bounty granting 0 resources, #5148).
  --
  -- QuietlyRemoveFromGame drops the entity outright, which is the state
  -- clearRemovedEntities leaves behind. RemoveEnemy alone only sets the
  -- placement, leaving it queryable, and this test would pass without the fix.
  it "reads the health of an enemy that is gone from the entity map" . gameTest $ \self -> do
    withProp @"combat" 1 self
    enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `fightEnemy` enemy
    startSkillTest
    click "Apply results"
    run $ QuietlyRemoveFromGame (toTarget enemy)
    Helpers.getEnemyField EnemyHealthActual (toId enemy) `shouldReturn` Nothing
    getDefeatedEnemyHealth (toId enemy) `shouldReturn` Just 1
