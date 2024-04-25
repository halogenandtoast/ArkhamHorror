module Arkham.Asset.Cards.Shotgun4Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

{- Fight. You get +3 combat for this attack. Instead of its standard damage, this attack deals 1 damage for each point you succeed by (to a minimum of 1, to a maximum of 5). If you fail and would damage another investigator, this attack deals 1 damage for each point you fail by (to a minimum of 1, to a maximum of 5). #-}

spec :: Spec
spec = describe "Shotgun (4)" $ do
  hasUses @"ammo" Assets.shotgun4 2

  context "Fight action" $ do
    let
      setup self enemy = do
        shotgun4 <- self `putAssetIntoPlay` Assets.shotgun4
        location <- testLocation
        setChaosTokens [Zero]
        self `moveTo` location
        enemy `spawnAt` location
        [doFight] <- shotgun4.abilities
        self `useAbility` doFight
        click "choose enemy"
        click "start skill test"

    it "gives +3 combat" . gameTest $ \self -> do
      withProp @"combat" 0 self
      enemy <- testEnemy
      setup self enemy
      self.combat `shouldReturn` 3

    context "gives +1 damage for each point you succeed" $ do
      it "to a minimum of 1" . gameTest $ \self -> do
        withProp @"combat" 0 self
        enemy <- testEnemy & prop @"fight" 3 & prop @"health" 6
        -- will pass by 0
        setup self enemy
        click "apply results"
        -- 1 minimum
        enemy.damage `shouldReturn` 1

      it "to a maximum of 5" . gameTest $ \self -> do
        withProp @"combat" 3 self
        enemy <- testEnemy & prop @"fight" 0 & prop @"health" 7
        -- will pass by 6
        setup self enemy
        click "apply results"
        -- 1 regular + 5 maximum
        enemy.damage `shouldReturn` 6

    context "if you fail and would damage another investigator" $ do
      context "gives 1 damage for each point you fail" $ do
        it "to a minimum of 1" . gameTest $ \self -> do
          withProp @"combat" 3 self
          roland <- addInvestigator rolandBanks
          enemy <- testEnemy & prop @"fight" 0 & prop @"health" 7
          shotgun4 <- self `putAssetIntoPlay` Assets.shotgun4
          location <- testLocation
          setChaosTokens [AutoFail]
          roland `moveTo` location
          enemy `spawnAt` location
          self `moveTo` location
          [doFight] <- shotgun4.abilities
          self `useAbility` doFight
          click "choose enemy"
          click "start skill test"
          click "apply results"
          applyAllDamage
          enemy.damage `shouldReturn` 0
          roland.damage `shouldReturn` 2

        it "to a maximum of 5" . gameTest $ \self -> do
          withProp @"combat" 0 self
          roland <- addInvestigator rolandBanks
          enemy <- testEnemy & prop @"fight" 6 & prop @"health" 7
          shotgun4 <- self `putAssetIntoPlay` Assets.shotgun4
          location <- testLocation
          setChaosTokens [AutoFail]
          roland `moveTo` location
          enemy `spawnAt` location
          self `moveTo` location
          [doFight] <- shotgun4.abilities
          self `useAbility` doFight
          click "choose enemy"
          click "start skill test"
          click "apply results"
          applyAllDamage
          enemy.damage `shouldReturn` 0
          roland.damage `shouldReturn` 5
