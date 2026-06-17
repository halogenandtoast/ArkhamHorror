module Arkham.Enemy.Cards.StalkingHybridSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Stalking Hybrid" do
  isHunter Enemies.stalkingHybrid

  it "spawns unengaged when drawn by an investigator that does not control the Vale Lantern"
    . gameTest
    $ \self -> do
      location <- testLocation
      self `moveTo` location
      stalkingHybrid <- testEnemyWithDef Enemies.stalkingHybrid id
      run $ InvestigatorDrawEnemy (toId self) (toId stalkingHybrid)
      self.engagedEnemies `shouldReturn` []
      assertAny $ enemyAt (toId location) <> enemyIs Enemies.stalkingHybrid
