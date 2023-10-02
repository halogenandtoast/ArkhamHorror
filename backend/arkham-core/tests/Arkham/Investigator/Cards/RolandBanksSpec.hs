module Arkham.Investigator.Cards.RolandBanksSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

spec :: Spec
spec = describe "Roland Banks" $ do
  context "after defeating an enemy" $ do
    it "reaction discovers a clue at your location" . gameTestWith rolandBanks $ \self -> do
      enemy <- testEnemy & prop @"fight" 4 & prop @"health" 1
      location <- testLocation & prop @"clues" 1
      setChaosTokens [Zero]
      enemy `spawnAt` location
      self `moveTo` location
      self `fightEnemy` enemy
      click "start skill test"
      click "apply results"
      useReaction
      self.clues `shouldReturn` 1

    faq "by any card under your control" $ do
      it "reaction discovers a clue at your location" . gameTestWith rolandBanks $ \self -> do
        enemy <- testEnemy & prop @"health" 1 & prop @"healthDamage" 1
        location <- testLocation & prop @"clues" 1
        guardDog <- self `putAssetIntoPlay` Assets.guardDog
        enemy `spawnAt` location
        self `moveTo` location
        self `attackedBy` enemy
        self `assignDamageTo` guardDog
        useReaction -- guard dog reaction
        useReaction -- roland banks reaction
        self.clues `shouldReturn` 1

    faq "can only discover if there is a clue on your location" $ do
      it "doesn't trigger if there are no clues" . gameTestWith rolandBanks $ \self -> do
        enemy <- testEnemy & prop @"fight" 4 & prop @"health" 1
        location <- testLocation & prop @"clues" 0
        setChaosTokens [Zero]
        enemy `spawnAt` location
        self `moveTo` location
        self `fightEnemy` enemy
        click "start skill test"
        click "apply results"
        assertHasNoReaction

  context "elder sign" $ do
    it "gives +1 for each clue on your location" . gameTestWith rolandBanks $ \self -> do
      location <- testLocation & prop @"clues" 4
      setChaosTokens [ElderSign]
      self `moveTo` location
      self.elderSignModifier `shouldReturn` PositiveModifier 4
