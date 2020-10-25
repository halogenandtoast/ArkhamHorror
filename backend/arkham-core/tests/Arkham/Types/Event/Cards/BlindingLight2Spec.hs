module Arkham.Types.Event.Cards.BlindingLight2Spec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = do
  describe "Blinding Light 2" $ do
    it "Uses willpower to evade an enemy" $ do
      scenario' <- testScenario "00000" id
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorWillpower = 5, investigatorAgility = 3 }
      enemy <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
      blindingLight2 <- buildEvent "01069" investigator
      location <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight2
          ]
          ((events %~ insertEntity blindingLight2)
          . (enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 2 damage to the evaded enemy" $ do
      scenario' <- testScenario "00000" id
      investigator <- testInvestigator "00000" id
      enemy <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
      blindingLight2 <- buildEvent "01069" investigator
      location <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight2
          ]
          ((events %~ insertEntity blindingLight2)
          . (enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
      updated game enemy `shouldSatisfy` hasDamage (2, 0)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action and takes 1 horror"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          scenario' <- testScenario "00000" id
          investigator <- testInvestigator "00000" id
          enemy <- testEnemy
            (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
          blindingLight2 <- buildEvent "01069" investigator
          location <- testLocation "00000" id
          game <-
            runGameTest
              investigator
              [ SetTokens [token]
              , enemySpawn location enemy
              , moveTo investigator location
              , playEvent investigator blindingLight2
              ]
              ((events %~ insertEntity blindingLight2)
              . (enemies %~ insertEntity enemy)
              . (locations %~ insertEntity location)
              . (scenario ?~ scenario')
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
            >>= runGameTestOnlyOption "take event damage"
          blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
          investigator `shouldSatisfy` hasRemainingActions game 2
          updated game investigator `shouldSatisfy` hasDamage (0, 1)
