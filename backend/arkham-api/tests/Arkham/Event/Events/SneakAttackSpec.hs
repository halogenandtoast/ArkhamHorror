module Arkham.Event.Events.SneakAttackSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Sneak Attack" $ do
  it "deals 2 damage to an exhausted enemy at your location" . gameTest $ \self -> do
    location <- testLocation
    enemy1 <- testEnemy & prop @"health" 3 & exhausted
    enemy2 <- testEnemy & prop @"health" 3 & exhausted
    enemy3 <- testEnemy & prop @"health" 3
    self `moveTo` location
    enemy1 `spawnAt` location
    enemy2 `spawnAt` location
    enemy3 `spawnAt` location
    self `playEvent` Events.sneakAttack
    assertTarget enemy2
    assertNotTarget enemy3
    chooseTarget enemy1
    enemy1.damage `shouldReturn` 2
    enemy2.damage `shouldReturn` 0
    enemy3.damage `shouldReturn` 0

  -- Regression for issue #4794: another investigator's Words of Power (which
  -- only forbids *its owner* from damaging doomed enemies at the location) was
  -- wrongly blocking *your* Sneak Attack from being playable, because the
  -- playability check substitutes ThisCard with a CardCostSource that was not
  -- recognized as a player card.
  it "is playable against an enemy only another investigator is forbidden from damaging" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    location <- testLocation
    enemy1 <- testEnemy & prop @"health" 3 & exhausted
    self `moveTo` location
    roland `moveTo` location
    enemy1 `spawnAt` location
    run
      =<< gameModifier
        (toSource roland)
        (toTarget enemy1)
        (CannotBeDamagedByPlayerSources (SourceOwnedBy (InvestigatorWithId (toId roland))))
    sneakAttack <- genPlayerCardWith Events.sneakAttack (setPlayerCardOwner (toId self))
    withProp @"resources" 10 self
    addToHand self sneakAttack
    duringTurn self $ asDefs self.playableCards `shouldReturn` [Events.sneakAttack]
