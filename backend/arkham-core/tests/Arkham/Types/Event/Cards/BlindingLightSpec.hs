module Arkham.Types.Event.Cards.BlindingLightSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      scenario' <- testScenario "00000" id
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorWillpower = 5, investigatorAgility = 3 }
      enemy <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
      blindingLight <- buildEvent "01066" investigator
      location <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight
          ]
          ((events %~ insertEntity blindingLight)
          . (enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 1 damage to the evaded enemy" $ do
      scenario' <- testScenario "00000" id
      investigator <- testInvestigator "01004" id
      enemy <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
      blindingLight <- buildEvent "01066" investigator
      location <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight
          ]
          ((events %~ insertEntity blindingLight)
          . (enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
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
          scenario' <- testScenario "00000" id
          investigator <- testInvestigator "01004" id
          enemy <- testEnemy
            (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
          blindingLight <- buildEvent "01066" investigator
          location <- testLocation "00000" id
          game <-
            runGameTest
              investigator
              [ enemySpawn location enemy
              , moveTo investigator location
              , playEvent investigator blindingLight
              ]
              ((events %~ insertEntity blindingLight)
              . (enemies %~ insertEntity enemy)
              . (locations %~ insertEntity location)
              . (chaosBag .~ Bag [token])
              . (scenario ?~ scenario')
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
          blindingLight `shouldSatisfy` isInDiscardOf game investigator
          investigator `shouldSatisfy` hasRemainingActions game 2
