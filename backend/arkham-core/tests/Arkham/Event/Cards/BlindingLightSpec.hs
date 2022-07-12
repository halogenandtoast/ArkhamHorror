{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Event.Cards.BlindingLightSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Attrs qualified as EnemyAttrs
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Enemy.Attrs (Field(..))

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      investigator <- testJenny $ \attrs ->
        attrs { investigatorWillpower = 5, investigatorAgility = 3 }
      enemy <- testEnemy
        (set EnemyAttrs.evadeL 4 . set EnemyAttrs.healthL (Static 2))
      blindingLight <- buildEvent "01066" investigator
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight
          ]
          ((entitiesL . eventsL %~ insertEntity blindingLight)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"

            isInDiscardOf investigator blindingLight `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True

    it "deals 1 damage to the evaded enemy" $ do
      investigator <- testJenny id
      enemy <- testEnemy
        ((EnemyAttrs.evadeL .~ 4) . (EnemyAttrs.healthL .~ Static 2))
      blindingLight <- buildEvent "01066" investigator
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight
          ]
          ((entitiesL . eventsL %~ insertEntity blindingLight)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"

            isInDiscardOf investigator blindingLight `shouldReturn` True
            fieldAssert EnemyDamage (== 1) enemy

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          investigator <- testJenny id
          enemy <- testEnemy
            ((EnemyAttrs.evadeL .~ 4) . (EnemyAttrs.healthL .~ Static 2))
          blindingLight <- buildEvent "01066" investigator
          location <- testLocation id
          gameTest
              investigator
              [ SetTokens [token]
              , enemySpawn location enemy
              , moveTo investigator location
              , playEvent investigator blindingLight
              ]
              ((entitiesL . eventsL %~ insertEntity blindingLight)
              . (entitiesL . enemiesL %~ insertEntity enemy)
              . (entitiesL . locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                chooseOnlyOption "Evade enemy"
                chooseOnlyOption "Run skill check"
                chooseOnlyOption "Apply results"

                isInDiscardOf investigator blindingLight `shouldReturn` True
                getRemainingActions investigator `shouldReturn` 2
