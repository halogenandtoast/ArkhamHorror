module Arkham.Event.Cards.CunningDistractionSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Card.CardDef qualified as CardDef
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Keyword

spec :: Spec
spec = do
  describe "Cunning Distraction" $ do
    it "Evades enemies engaged with you" $ do
      investigator <- testJenny id
      location <- testLocation id
      enemy <- testEnemy id
      cunningDistraction <- buildEvent "01078" investigator
      gameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator cunningDistraction
          ]
          ((entitiesL . eventsL %~ insertEntity cunningDistraction)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            isInDiscardOf investigator cunningDistraction `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True

    it "Evades enemies engaged with other investigators at your location" $ do
      investigator <- testJenny id
      investigator2 <- testInvestigator Investigators.rolandBanks id
      location <- testLocation id
      enemy <- testEnemy id
      cunningDistraction <- buildEvent "01078" investigator
      gameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator2 location -- move investigator 2 first to engage
          , moveTo investigator location
          , playEvent investigator cunningDistraction
          ]
          ((entitiesL . eventsL %~ insertEntity cunningDistraction)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            isInDiscardOf investigator cunningDistraction `shouldReturn` True
            evadedBy investigator2 enemy `shouldReturn` True

    it "Evades aloof enemies at your location" $ do
      investigator <- testJenny id
      location <- testLocation id
      enemy <- testEnemyWithDef (CardDef.keywordsL .~ setFromList [Aloof]) id
      cunningDistraction <- buildEvent "01078" investigator
      gameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator cunningDistraction
          ]
          ((entitiesL . eventsL %~ insertEntity cunningDistraction)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            isInDiscardOf investigator cunningDistraction `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True
