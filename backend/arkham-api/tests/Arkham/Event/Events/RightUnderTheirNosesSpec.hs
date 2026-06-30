module Arkham.Event.Events.RightUnderTheirNosesSpec (spec) where

-- import Arkham.Classes.HasGame (getGame)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Location.Cards qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Right Under Their Noses" $ do
  -- Regression for #4970: evading exhausts the enemy, which must resolve before
  -- the "after you evade" reaction window. Padma Amrita blocks clue discovery at
  -- Ancient locations only "while ready", so once evaded a clue is discoverable.
  it "discovers a clue after evading a 'while ready' discovery-blocker" . gameTest $ \self -> do
    withProp @"agility" 1 self
    withProp @"resources" 2 self
    location <- testLocationWithDef Locations.entryway id & prop @"clues" 1
    padma <- testEnemyWithDef Enemies.padmaAmrita id & prop @"evade" 0
    rightUnderTheirNoses <- genCard Events.rightUnderTheirNoses
    self `addToHand` rightUnderTheirNoses
    self `moveTo` location
    padma `spawnAt` location
    setChaosTokens [Zero]
    self `evadeEnemy` padma
    startSkillTest
    applyResults
    chooseTarget rightUnderTheirNoses
    assert padma.exhausted
    self.clues `shouldReturn` 1
    location.clues `shouldReturn` 0
