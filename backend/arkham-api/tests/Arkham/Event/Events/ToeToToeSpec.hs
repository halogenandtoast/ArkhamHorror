module Arkham.Event.Events.ToeToToeSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Keyword qualified as Keyword
import Arkham.Modifier
import TestImport.New

spec :: Spec
spec = describe "Toe to Toe" do
  -- As an additional cost the chosen enemy makes an attack against you; a forced
  -- attack happens even while the enemy is exhausted.
  it "forces the chosen enemy to attack you even while it is exhausted" . gameTest $ \self -> do
    withProp @"combat" 4 self
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3 & prop @"healthDamage" 1 & exhausted
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    toeToToe <- genCard Events.toeToToe
    self `addToHand` toeToToe
    self `playCard` toeToToe
    chooseTarget enemy
    -- the chosen enemy attacks as an additional cost; assign its damage
    click "assign damage from the forced attack"
    startSkillTest
    applyResults
    self.damage `shouldReturn` 1
    enemy.damage `shouldReturn` 2

  -- #5324: the additional-cost attack lets Elusive move the chosen enemy away
  -- before the play finishes resolving. The play is already initiated, so the
  -- fight still resolves against that enemy (FAQ v2.5 Q112, Q086) -- it must not
  -- fizzle back into hand.
  it "still fights an elusive enemy that flees after the additional-cost attack" . gameTest $ \self -> do
    withProp @"combat" 4 self
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3 & prop @"healthDamage" 1
    (here, there) <- testConnectedLocations id id
    setChaosTokens [Zero]
    enemy `spawnAt` here
    self `moveTo` here
    run =<< gameModifier (TestSource mempty) (toTarget enemy) (AddKeyword Keyword.Elusive)
    toeToToe <- genCard Events.toeToToe
    self `addToHand` toeToToe
    self `playCard` toeToToe
    chooseTarget enemy
    click "assign damage from the forced attack"
    -- Elusive: the enemy disengages, flees to the only connecting location, and exhausts
    enemy.location `shouldReturn` Just (toId there)
    startSkillTest
    applyResults
    enemy.damage `shouldReturn` 2
    assert $ Events.toeToToe `isInDiscardOf` self
