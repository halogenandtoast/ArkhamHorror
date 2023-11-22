module Arkham.Enemy.Cards.StubbornDetectiveSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import TestImport.New

spec :: Spec
spec = describe "Stubborn Detective" $ do
  isHunter Enemies.stubbornDetective

  it "while at your location treat your card as blank" . gameTest $ \self -> do
    (location1, location2) <- testConnectedLocations id id
    self `moveTo` location1
    stubbornDetective <- createWeaknessEnemy self Enemies.stubbornDetective
    stubbornDetective `spawnAt` location2
    getModifiers self `shouldNotContainM` [Blank]
    run HuntersMove
    getModifiers self `shouldReturn` [Blank]
