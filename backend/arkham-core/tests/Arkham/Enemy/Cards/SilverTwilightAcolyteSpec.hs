module Arkham.Enemy.Cards.SilverTwilightAcolyteSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import TestImport.New

spec :: Spec
spec = describe "Silver Twilight Acolyte" $ do
  isHunter Enemies.silverTwilightAcolyte

  context "After Silver Twilight Acolyte attacks" $ do
    it "places 1 doom on the current agenda" . gameTest $ \self -> do
      agenda <- testAgenda "01105" id
      location <- testLocation
      self `moveTo` location
      silverTwilightAcolyte <- createWeaknessEnemy self Enemies.silverTwilightAcolyte
      silverTwilightAcolyte `spawnAt` location
      run EnemiesAttack
      applyAllDamage
      useForcedAbility
      agenda.doom `shouldReturn` 1
