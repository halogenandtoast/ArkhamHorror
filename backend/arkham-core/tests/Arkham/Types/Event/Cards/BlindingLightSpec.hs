module Arkham.Types.Event.Cards.BlindingLightSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorWillpower = 5, investigatorAgility = 3 }
      enemy <- testEnemy
        (set EnemyAttrs.evadeL 4 . set EnemyAttrs.healthL (Static 2))
      blindingLight <- buildEvent "01066" investigator
      location <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight
          ]
          ((eventsL %~ insertEntity blindingLight)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 1 damage to the evaded enemy" $ do
      investigator <- testInvestigator "01004" id
      enemy <- testEnemy
        ((EnemyAttrs.evadeL .~ 4) . (EnemyAttrs.healthL .~ Static 2))
      blindingLight <- buildEvent "01066" investigator
      location <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight
          ]
          ((eventsL %~ insertEntity blindingLight)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game investigator
      updated game enemy `shouldSatisfy` hasDamage (1, 0)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          investigator <- testInvestigator "01004" id
          enemy <- testEnemy
            ((EnemyAttrs.evadeL .~ 4) . (EnemyAttrs.healthL .~ Static 2))
          blindingLight <- buildEvent "01066" investigator
          location <- testLocation "00000" id
          game <-
            runGameTest
              investigator
              [ SetTokens [token]
              , enemySpawn location enemy
              , moveTo investigator location
              , playEvent investigator blindingLight
              ]
              ((eventsL %~ insertEntity blindingLight)
              . (enemiesL %~ insertEntity enemy)
              . (locationsL %~ insertEntity location)
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
          blindingLight `shouldSatisfy` isInDiscardOf game investigator
          investigator `shouldSatisfy` hasRemainingActions game 2
