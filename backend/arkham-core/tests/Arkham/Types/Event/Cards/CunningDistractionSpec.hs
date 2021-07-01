module Arkham.Types.Event.Cards.CunningDistractionSpec
  ( spec
  )
where

import TestImport.Lifted

import qualified Arkham.Types.Card.CardDef as CardDef
import Arkham.Types.Keyword

spec :: Spec
spec = do
  describe "Cunning Distraction" $ do
    it "Evades enemies engaged with you" $ do
      investigator <- testInvestigator "00000" id
      location <- testLocation id
      enemy <- testEnemy id
      cunningDistraction <- buildEvent "01078" investigator
      gameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator cunningDistraction
          ]
          ((eventsL %~ insertEntity cunningDistraction)
          . (locationsL %~ insertEntity location)
          . (enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            isInDiscardOf investigator cunningDistraction `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True

    it "Evades enemies engaged with other investigators at your location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
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
          ((eventsL %~ insertEntity cunningDistraction)
          . (locationsL %~ insertEntity location)
          . (enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            isInDiscardOf investigator cunningDistraction `shouldReturn` True
            evadedBy investigator2 enemy `shouldReturn` True

    it "Evades aloof enemies at your location" $ do
      investigator <- testInvestigator "00000" id
      location <- testLocation id
      enemy <- testEnemyWithDef (CardDef.keywordsL .~ setFromList [Aloof]) id
      cunningDistraction <- buildEvent "01078" investigator
      gameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator cunningDistraction
          ]
          ((eventsL %~ insertEntity cunningDistraction)
          . (locationsL %~ insertEntity location)
          . (enemiesL %~ insertEntity enemy)
          )
        $ do
            runMessages
            isInDiscardOf investigator cunningDistraction `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True
