module Arkham.Event.Events.BountySpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Bounty" do
  -- the regression guard for the enemy being gone by the time this resolves
  -- lives in Arkham.Helpers.EnemySpec; the test harness keeps the defeated
  -- enemy queryable, so it cannot reproduce that condition here (#5148)
  it "takes resources equal to the defeated enemy's health" . gameTest $ \self -> do
    withProp @"combat" 1 self
    withProp @"resources" 0 self
    enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
    location <- testLocation
    bounty <- genCard Events.bounty
    setChaosTokens [Zero]
    self `addToHand` bounty
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `fightEnemy` enemy
    startSkillTest
    click "Apply results"
    chooseTarget $ toCardId bounty
    self.resources `shouldReturn` 1
