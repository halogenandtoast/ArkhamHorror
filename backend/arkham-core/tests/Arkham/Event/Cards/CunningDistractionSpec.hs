module Arkham.Event.Cards.CunningDistractionSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = do
  describe "Cunning Distraction" $ do
    it "Evades enemies engaged with you" . gameTest $ \self -> do
      location <- testLocation
      enemy <- testEnemy
      enemy `spawnAt` location
      self `moveTo` location
      self `putCardIntoPlay` Events.cunningDistraction
      assert $ Events.cunningDistraction `isInDiscardOf` self
      assert $ enemy `evadedBy` self

    it "Evades enemies engaged with other investigators at your location" . gameTest $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks
      location <- testLocation
      enemy <- testEnemy
      enemy `spawnAt` location
      investigator2 `moveTo` location -- move investigator 2 first to engage
      self `moveTo` location
      self `putCardIntoPlay` Events.cunningDistraction
      assert $ Events.cunningDistraction `isInDiscardOf` self
      assert $ enemy `evadedBy` investigator2

    it "Evades aloof enemies at your location" . gameTest $ \self -> do
      location <- testLocation
      enemy <- testEnemyWithDef Enemies.whippoorwill id
      enemy `spawnAt` location
      self `moveTo` location
      self `putCardIntoPlay` Events.cunningDistraction
      assert $ Events.cunningDistraction `isInDiscardOf` self
      assert $ enemy `evadedBy` self
