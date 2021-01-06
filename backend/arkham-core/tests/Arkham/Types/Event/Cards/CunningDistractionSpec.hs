module Arkham.Types.Event.Cards.CunningDistractionSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Keyword

spec :: Spec
spec = do
  describe "Cunning Distraction" $ do
    it "Evades enemies engaged with you" $ do
      investigator <- testInvestigator "00000" id
      location <- testLocation "00000" id
      enemy <- testEnemy id
      cunningDistraction <- buildEvent "01078" investigator
      game <- runGameTest
        investigator
        [ enemySpawn location enemy
        , moveTo investigator location
        , playEvent investigator cunningDistraction
        ]
        ((eventsL %~ insertEntity cunningDistraction)
        . (locationsL %~ insertEntity location)
        . (enemiesL %~ insertEntity enemy)
        )
      cunningDistraction `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "Evades enemies engaged with other investigators at your location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      location <- testLocation "00000" id
      enemy <- testEnemy id
      cunningDistraction <- buildEvent "01078" investigator
      game <- runGameTest
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
      cunningDistraction `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator2

    it "Evades aloof enemies at your location" $ do
      investigator <- testInvestigator "00000" id
      location <- testLocation "00000" id
      enemy <- testEnemy (EnemyAttrs.keywordsL .~ setFromList [Aloof])
      cunningDistraction <- buildEvent "01078" investigator
      game <- runGameTest
        investigator
        [ enemySpawn location enemy
        , moveTo investigator location
        , playEvent investigator cunningDistraction
        ]
        ((eventsL %~ insertEntity cunningDistraction)
        . (locationsL %~ insertEntity location)
        . (enemiesL %~ insertEntity enemy)
        )
      cunningDistraction `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator
