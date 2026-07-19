module Arkham.Event.Events.GuerrillaTacticsSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Guerrilla Tactics" do
  it "cannot evade an enemy immune to player card effects" . gameTest $ \self -> do
    (here, connecting) <- testConnectedLocations id id
    self `moveTo` here
    felineHybrid <- testEnemyWithDef Enemies.felineHybrid id
    validEnemy <- testEnemy
    felineHybrid `spawnAt` connecting
    validEnemy `spawnAt` connecting
    guerrillaTactics <- genCard Events.guerrillaTactics
    withProp @"resources" 1 self
    self `addToHand` guerrillaTactics

    duringTurn self do
      self `playCard` guerrillaTactics
      clickLabel "$cards.label.guerrillaTactics.evade"
      assertNotTarget felineHybrid
      assertTarget validEnemy
