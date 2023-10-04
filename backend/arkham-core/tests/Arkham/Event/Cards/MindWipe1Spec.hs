module Arkham.Event.Cards.MindWipe1Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Mind Wipe (1)" $ do
  it "treats an enemy's printed text box as blank until the end of the phase" . gameTest $ \self -> do
    withProp @"resources" 1 self
    mindWipe1 <- genCard Events.mindWipe1
    self `addToHand` mindWipe1
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    duringPhase #investigation $ do
      chooseTarget mindWipe1
      chooseTarget enemy
      getModifiers enemy `shouldReturn` [Blank]
    getModifiers enemy `shouldReturn` []
