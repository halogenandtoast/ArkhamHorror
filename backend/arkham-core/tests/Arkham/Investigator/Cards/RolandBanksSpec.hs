module Arkham.Investigator.Cards.RolandBanksSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Investigator.Types (Field(..))
import Arkham.Enemy.Types (EnemyAttrs(..))
import Arkham.Location.Types (LocationAttrs(..))

spec :: Spec
spec = describe "Roland Banks" $ do
  context "ability" $ do
    it
        "after defeating an enemy, allows you to discover a clue at your location"
      $ do
          let rolandBanks = lookupInvestigator "01001"
          enemy <- testEnemy
            $ \attrs -> attrs { enemyFight = 4, enemyHealth = Static 1 }
          location <- testLocation $ \attrs -> attrs { locationClues = 1 }
          gameTest
              rolandBanks
              [ SetTokens [Zero]
              , enemySpawn location enemy
              , moveTo rolandBanks location
              , fightEnemy rolandBanks enemy
              ]
              ((entitiesL . enemiesL %~ insertEntity enemy)
              . (entitiesL . locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                chooseOnlyOption "start skill test"
                chooseOnlyOption "apply results"
                chooseOptionMatching
                  "use ability"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
                fieldAssert InvestigatorClues (== 1) rolandBanks

  context "elder sign" $ do
    it "gives +1 for each clue on your location" $ do
      let rolandBanks = lookupInvestigator "01001"
      location <- testLocation
        $ \attrs -> attrs { locationClues = 1, locationShroud = 4 }
      gameTest
          rolandBanks
          [ SetTokens [ElderSign]
          , moveTo rolandBanks location
          , investigate rolandBanks location
          ]
          (entitiesL . locationsL %~ insertEntity location)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            fieldAssert InvestigatorClues (== 1) rolandBanks
