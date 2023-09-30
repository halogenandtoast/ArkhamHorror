module Arkham.Event.Cards.CunningDistractionSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = do
  describe "Cunning Distraction" $ do
    it "Evades enemies engaged with you" $ gameTest $ \investigator -> do
      location <- testLocationWith id
      enemy <- testEnemyWith id
      pushAndRunAll
        [ spawnAt enemy location
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.cunningDistraction
      assert $ Events.cunningDistraction `isInDiscardOf` investigator
      assert $ evadedBy investigator enemy

    it "Evades enemies engaged with other investigators at your location" $ gameTest $ \investigator -> do
      investigator2 <- addInvestigator Investigators.rolandBanks id
      location <- testLocationWith id
      enemy <- testEnemyWith id
      pushAndRunAll
        [ spawnAt enemy location
        , moveTo investigator2 location -- move investigator 2 first to engage
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.cunningDistraction
      assert $ Events.cunningDistraction `isInDiscardOf` investigator
      assert $ evadedBy investigator2 enemy

    it "Evades aloof enemies at your location" $ gameTest $ \investigator -> do
      location <- testLocationWith id
      enemy <- testEnemyWithDef Enemies.whippoorwill id
      pushAndRunAll
        [ spawnAt enemy location
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.cunningDistraction
      assert $ Events.cunningDistraction `isInDiscardOf` investigator
      assert $ evadedBy investigator enemy
