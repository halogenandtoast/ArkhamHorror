module Arkham.Event.Events.Marksmanship1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

-- Marksmanship (1) has an in-hand HasModifiersFor that "opens up" a
-- Firearm/Ranged fight action to target enemies at a connected location. The
-- weapon is put into play *after* the event is placed so the next message's
-- preloadEntities pass picks the event up as an in-hand effect entity.
spec :: Spec
spec = describe "Marksmanship (1)" do
  it "while in hand, lets a Firearm fight an enemy at a connected location"
    . gameTest
    $ \self -> do
      withProp @"combat" 5 self
      withProp @"resources" 5 self
      setChaosTokens [Zero]

      (here, there) <- testConnectedLocations id id
      self `moveTo` here
      run $ RevealLocation Nothing (toId there)
      enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
      enemy `spawnAt` there

      marksmanship <- genMyCard self Events.marksmanship1
      withProp @"hand" [marksmanship] self
      fortyFiveAutomatic <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic

      [doFight] <- self `getActionsFrom` fortyFiveAutomatic
      self `useAbility` doFight
      chooseTarget marksmanship -- play Marksmanship from the fight window
      chooseTarget enemy -- its effect lets the fight target the connected enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 3

  it "attached under Stick to the Plan (3), still lets a Firearm fight an enemy at a connected location"
    . gameTest
    $ \self -> do
      withProp @"combat" 5 self
      withProp @"resources" 5 self
      setChaosTokens [Zero]

      (here, there) <- testConnectedLocations id id
      self `moveTo` here
      run $ RevealLocation Nothing (toId there)
      enemy <- testEnemy & prop @"fight" 2 & prop @"health" 5
      enemy `spawnAt` there

      stickToThePlan <- self `putAssetIntoPlay` Assets.stickToThePlan3
      marksmanship <- genMyCard self Events.marksmanship1
      run $ PlaceUnderneath (toTarget stickToThePlan) [marksmanship]
      fortyFiveAutomatic <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic

      [doFight] <- self `getActionsFrom` fortyFiveAutomatic
      self `useAbility` doFight
      chooseTarget marksmanship -- play Marksmanship from the fight window
      chooseTarget enemy -- its effect lets the fight target the connected enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 3
