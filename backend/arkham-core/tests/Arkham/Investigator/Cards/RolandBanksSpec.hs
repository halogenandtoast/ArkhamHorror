{-# OPTIONS_GHC -Wno-type-defaults #-}

module Arkham.Investigator.Cards.RolandBanksSpec (spec) where

import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

default (Int)

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

  context "elder sign" $ do
    it "gives +1 for each clue on your location" . gameTestWith rolandBanks $ \self -> do
      location <- testLocation & prop @"clues" 4
      setChaosTokens [ElderSign]
      self `moveTo` location
      self.elderSignModifier `shouldReturn` PositiveModifier 4
