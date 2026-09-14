module Arkham.Enemy.Cards.TheDrownedCity.StarSpawn.CoralStarSpawnSpec (spec) where

import Arkham.Enemy.CardDefs.TheDrownedCity.StarSpawn qualified as Enemies
import Arkham.Enemy.Types
import Arkham.Phase
import TestImport.New

spec :: Spec
spec = describe "Coral Star Spawn" do
  -- #5589: the second relentless attack used to be queued alongside the first, so
  -- another enemy attacking in the same phase could reorder the pair, exhaust the
  -- Star Spawn early and swallow one of the two attacks.
  it "attacks a second time even when another enemy attacks in the same phase" . gameTest $ \self -> do
    location <- testLocation
    coralStarSpawn <- testEnemyWithDef Enemies.coralStarSpawn id & prop @"healthDamage" 1
    other <- testEnemyWith \attrs -> attrs {enemySanityDamage = 1}
    coralStarSpawn `spawnAt` location
    other `spawnAt` location
    self `moveTo` location

    run $ SetPhase EnemyPhase
    run EnemiesAttack
    chooseTarget coralStarSpawn
    applyAllDamage
    chooseTarget other
    applyAllHorror
    run RelentlessEnemiesAttack
    chooseTarget coralStarSpawn
    applyAllDamage

    self.damage `shouldReturn` 2
    self.horror `shouldReturn` 1
    coralStarSpawn.exhausted `shouldReturn` True
