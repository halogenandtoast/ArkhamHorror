{-# OPTIONS_GHC -Wno-type-defaults #-}

module Arkham.Investigator.Cards.AgnesBakerSpec (spec) where

import Arkham.Investigator.Cards
import TestImport.New

default (Int)

spec :: Spec
spec = describe "Agnes Baker" $ do
  context "ability" $ do
    it "can deal 1 damage to an enemy at your location when taking horror" . gameTestWith agnesBaker $ \self -> do
      enemy <- testEnemy & prop @"health" 2
      location <- testLocation
      enemy `spawnAt` location
      self `moveTo` location
      self `addHorror` 1
      useReaction
      click "damage enemy"
      enemy.damage `shouldReturn` 1

  context "elder sign" $ do
    it "gives +1 for each horror on Agnes" . gameTestWith agnesBaker $ \self -> do
      self `addHorror` 2
      self.elderSignModifier `shouldReturn` PositiveModifier 2
