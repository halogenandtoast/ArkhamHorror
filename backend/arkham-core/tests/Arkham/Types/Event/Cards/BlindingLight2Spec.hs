module Arkham.Types.Event.Cards.BlindingLight2Spec
  ( spec
  )
where

import TestImport.Lifted

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = do
  describe "Blinding Light 2" $ do
    it "Uses willpower to evade an enemy" $ do
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorWillpower = 5, investigatorAgility = 3 }
      enemy <- testEnemy
        (set EnemyAttrs.evadeL 4 . set EnemyAttrs.healthL (Static 3))
      blindingLight2 <- buildEvent "01069" investigator
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight2
          ]
          ((eventsL %~ insertEntity blindingLight2)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"
            isInDiscardOf investigator blindingLight2 `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True

    it "deals 2 damage to the evaded enemy" $ do
      investigator <- testInvestigator "00000" id
      enemy <- testEnemy
        (set EnemyAttrs.evadeL 4 . set EnemyAttrs.healthL (Static 3))
      blindingLight2 <- buildEvent "01069" investigator
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight2
          ]
          ((eventsL %~ insertEntity blindingLight2)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"
            isInDiscardOf investigator blindingLight2 `shouldReturn` True
            updated enemy `shouldSatisfyM` hasDamage (2, 0)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action and takes 1 horror"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          investigator <- testInvestigator "00000" id
          enemy <- testEnemy
            ((EnemyAttrs.evadeL .~ 4) . (EnemyAttrs.healthL .~ Static 3))
          blindingLight2 <- buildEvent "01069" investigator
          location <- testLocation id
          gameTest
              investigator
              [ SetTokens [token]
              , enemySpawn location enemy
              , moveTo investigator location
              , playEvent investigator blindingLight2
              ]
              ((eventsL %~ insertEntity blindingLight2)
              . (enemiesL %~ insertEntity enemy)
              . (locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                chooseOnlyOption "Evade enemy"
                chooseOnlyOption "Run skill check"
                chooseOnlyOption "Apply results"
                chooseOnlyOption "take event damage"
                isInDiscardOf investigator blindingLight2 `shouldReturn` True
                getRemainingActions investigator `shouldReturn` 2
                updated investigator `shouldSatisfyM` hasDamage (0, 1)
