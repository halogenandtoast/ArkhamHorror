module Arkham.Event.Cards.CloseCall2Spec (spec) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Event.Cards qualified as Cards
import Arkham.Matcher (EnemyMatcher (AnyEnemy))
import Arkham.Scenario.Types (Field (..))
import TestImport.New

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" . gameTest $ \self -> do
    withProp @"resources" 2 self
    enemy <- testEnemy
    location <- testLocation
    closeCall2 <- genCard Cards.closeCall2
    self `addToHand` closeCall2
    self `moveTo` location
    enemy `spawnAt` location
    self `evadedEnemy` enemy
    chooseTarget closeCall2
    deckSize <- scenarioFieldMap ScenarioEncounterDeck length
    deckSize `shouldBe` (1 :: Int)
    selectCount AnyEnemy `shouldReturn` 0

  it "does not work on Elite enemies" . gameTest $ \self -> do
    withProp @"resources" 2 self
    enemy <- testEnemy & elite
    location <- testLocation
    closeCall2 <- genCard Cards.closeCall2
    self `addToHand` closeCall2
    self `moveTo` location
    enemy `spawnAt` location
    self `evadedEnemy` enemy
    assertNotTarget closeCall2

  it "does not work on weakness enemies" . gameTest $ \self -> do
    withProp @"resources" 2 self
    enemy <- testEnemyWithDef Cards.mobEnforcer id
    run $ SetBearer (toTarget enemy) (toId self)
    location <- testLocation
    closeCall2 <- genCard Cards.closeCall2
    self `addToHand` closeCall2
    self `moveTo` location
    enemy `spawnAt` location
    self `evadedEnemy` enemy
    assertNotTarget closeCall2
