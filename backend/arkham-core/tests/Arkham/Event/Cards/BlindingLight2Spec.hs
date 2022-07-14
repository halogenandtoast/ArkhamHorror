{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Event.Cards.BlindingLight2Spec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Attrs qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Attrs (Field(..), InvestigatorAttrs(..), willpowerL)
import Arkham.Enemy.Attrs (Field(..))
import Arkham.Projection

spec :: Spec
spec = do
  describe "Blinding Light 2" $ do
    it "Uses willpower to evade an enemy" $ do
      investigator <- testJenny $ \attrs ->
        attrs { investigatorWillpower = 5, investigatorAgility = 3 }
      enemy <- testEnemy
        (set EnemyAttrs.evadeL 4 . set EnemyAttrs.healthL (Static 3))
      blindingLight2 <- buildEvent Events.blindingLight2 investigator
      let Just blindingLight2Card = preview _PlayerCard (toCard $ toAttrs blindingLight2)
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight2
          ]
          ((entitiesL . eventsL %~ insertEntity blindingLight2)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"
            assert $ fieldP InvestigatorDiscard (== [blindingLight2Card]) (toId investigator)
            assert $ fieldP EnemyEngagedInvestigators null (toId enemy)

    it "deals 2 damage to the evaded enemy" $ do
      investigator <- testJenny (willpowerL .~ 5)
      enemy <- testEnemy
        (set EnemyAttrs.evadeL 4 . set EnemyAttrs.healthL (Static 3))
      blindingLight2 <- buildEvent Events.blindingLight2 investigator
      let Just blindingLight2Card = preview _PlayerCard (toCard $ toAttrs blindingLight2)
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator blindingLight2
          ]
          ((entitiesL . eventsL %~ insertEntity blindingLight2)
          . (entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "Evade enemy"
            chooseOnlyOption "Run skill check"
            chooseOnlyOption "Apply results"
            assert $ fieldP InvestigatorDiscard (== [blindingLight2Card]) (toId investigator)
            assert $ fieldP EnemyDamage (== 2) (toId enemy)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action and takes 1 horror"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          investigator <- testJenny (willpowerL .~ 5)
          enemy <- testEnemy
            ((EnemyAttrs.evadeL .~ 4) . (EnemyAttrs.healthL .~ Static 3))
          blindingLight2 <- buildEvent Events.blindingLight2 investigator
          let Just blindingLight2Card = preview _PlayerCard (toCard $ toAttrs blindingLight2)
          location <- testLocation id
          gameTest
              investigator
              [ SetTokens [token]
              , enemySpawn location enemy
              , moveTo investigator location
              , playEvent investigator blindingLight2
              ]
              ((entitiesL . eventsL %~ insertEntity blindingLight2)
              . (entitiesL . enemiesL %~ insertEntity enemy)
              . (entitiesL . locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                chooseOnlyOption "Evade enemy"
                chooseOnlyOption "Run skill check"
                chooseOnlyOption "Apply results"
                chooseOnlyOption "take event damage"
                fieldAssert InvestigatorDiscard (== [blindingLight2Card]) investigator
                fieldAssert InvestigatorRemainingActions (== 2) investigator
                fieldAssert InvestigatorHorror (== 1) investigator
