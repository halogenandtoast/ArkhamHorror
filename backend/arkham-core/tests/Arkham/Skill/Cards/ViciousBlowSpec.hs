module Arkham.Skill.Cards.ViciousBlowSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Vicious Blow" $ do
  it "does 1 extra damage when attack is successful" $ gameTest $ \self -> do
    withProp @"combat" 1 self
    enemy <- testEnemy & prop @"health" 3 & prop @"fight" 1
    location <- testLocation
    viciousBlow <- genCard Skills.viciousBlow
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    self `addToHand` viciousBlow
    self `fightEnemy` enemy
    chooseTarget viciousBlow
    startSkillTest
    click "Apply results"
    enemy.damage `shouldReturn` 2
