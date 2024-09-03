module Arkham.Asset.Cards.BrandOfCthugha1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Brand of Cthugha (1)" do
  faq "If attack misses" do
    it "and play Oops! then 0 damage will be dealt" . gameTest $ \self -> do
      withProp @"willpower" 0 self
      withProp @"resources" 2 self
      setChaosTokens [Zero]
      oops <- genCard Events.oops
      self `addToHand` oops
      enemy1 <- testEnemy & prop @"health" 3 & prop @"fight" 2
      enemy2 <- testEnemy
      brandOfCthugha1 <- self `putAssetIntoPlay` Assets.brandOfCthugha1
      location <- testLocation
      enemy1 `spawnAt` location
      enemy2 `spawnAt` location
      self `moveTo` location

      [doFight] <- brandOfCthugha1.abilities
      self `useAbility` doFight
      chooseSkill #willpower

      chooseTarget enemy1
      click "start skill test"
      applyResults
      chooseTarget oops
      enemy1.damage `shouldReturn` 0
      enemy2.damage `shouldReturn` 0

    it "damage dealt to another investigator is 0" . gameTest $ \self -> do
      withProp @"willpower" 0 self
      withProp @"resources" 2 self
      setChaosTokens [Zero]
      investigator2 <- addInvestigator Investigators.rolandBanks
      enemy <- testEnemy & prop @"health" 3 & prop @"fight" 2
      brandOfCthugha1 <- self `putAssetIntoPlay` Assets.brandOfCthugha1
      location <- testLocation
      enemy `spawnAt` location
      investigator2 `moveTo` location
      self `moveTo` location

      [doFight] <- brandOfCthugha1.abilities
      self `useAbility` doFight
      chooseSkill #willpower

      click "choose enemy"
      click "start skill test"
      applyResults
      enemy.damage `shouldReturn` 0
      investigator2.damage `shouldReturn` 0
