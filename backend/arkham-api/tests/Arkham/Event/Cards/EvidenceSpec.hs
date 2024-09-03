module Arkham.Event.Cards.EvidenceSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Evidence!" do
  it "discovers a clue at your location after you defeat an enemy" . gameTest $ \self -> do
    withProp @"combat" 1 self
    withProp @"resources" 1 self
    enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
    location <- testLocation & prop @"clues" 1
    evidence <- genCard Events.evidence
    setChaosTokens [Zero]
    self `addToHand` evidence
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `fightEnemy` enemy
    click "Begin skill test"
    click "Apply results"
    chooseTarget $ toCardId evidence
    self.clues `shouldReturn` 1
    location.clues `shouldReturn` 0
