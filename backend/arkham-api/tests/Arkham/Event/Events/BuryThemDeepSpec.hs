module Arkham.Event.Events.BuryThemDeepSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types
import TestImport.New

spec :: Spec
spec = describe "Bury Them Deep" do
  it "adds the defeated enemy (and itself) to the victory display" . gameTest $ \self -> do
    withProp @"combat" 1 self
    enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
    location <- testLocation
    buryThemDeep <- genCard Events.buryThemDeep
    setChaosTokens [Zero]
    self `addToHand` buryThemDeep
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `fightEnemy` enemy
    startSkillTest
    click "Apply results"
    chooseTarget $ toCardId buryThemDeep
    assert $ selectNone AnyInPlayEnemy
    scenarioField ScenarioVictoryDisplay `shouldSatisfyM` elem (toCard enemy)
    scenarioField ScenarioVictoryDisplay `shouldSatisfyM` elem (toCard buryThemDeep)
