module Arkham.Enemy.Cards.DagonsBroodSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import TestImport.New

spec :: Spec
spec = describe "Dagon's Brood" $ do
  it "places 1 doom on Dagon (Deep in Slumber) when an investigator becomes engaged with it"
    $ gameTest
    $ \investigator -> do
      location <- testLocation
      investigator `moveTo` location
      dagon <- testEnemyWithDef Enemies.dagonDeepInSlumberIntoTheMaelstrom id
      dagonsBrood <- testEnemyWithDef Enemies.dagonsBrood id
      dagon `spawnAt` location
      dagonsBrood `spawnAt` location
      pushAndRun $ engageEnemy investigator dagonsBrood
      useForcedAbility
      dagon.id.doom `shouldReturn` 1
