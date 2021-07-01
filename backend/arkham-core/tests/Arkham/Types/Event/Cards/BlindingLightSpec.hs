module Arkham.Types.Event.Cards.BlindingLightSpec
  ( spec
  )
where

import TestImport.Lifted

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorWillpower = 5, investigatorAgility = 3 }
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
          ((eventsL %~ insertEntity blindingLight)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"

            isInDiscardOf investigator blindingLight `shouldReturn` True
            evadedBy investigator enemy `shouldReturn` True

    it "deals 1 damage to the evaded enemy" $ do
      investigator <- testInvestigator "01004" id
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
          ((eventsL %~ insertEntity blindingLight)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"

            isInDiscardOf investigator blindingLight `shouldReturn` True
            updated enemy `shouldSatisfyM` hasDamage (1, 0)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          investigator <- testInvestigator "01004" id
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
              ((eventsL %~ insertEntity blindingLight)
              . (enemiesL %~ insertEntity enemy)
              . (locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                chooseOnlyOption "Evade enemy"
                chooseOnlyOption "Run skill check"
                chooseOnlyOption "Apply results"

                isInDiscardOf investigator blindingLight `shouldReturn` True
                getRemainingActions investigator `shouldReturn` 2
