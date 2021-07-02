module Arkham.Types.Investigator.Cards.RolandBanksSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Enemy.Attrs (EnemyAttrs(..))
import Arkham.Types.Location.Attrs (LocationAttrs(..))

spec :: Spec
spec = describe "Roland Banks" $ do
  context "ability" $ do
    it
        "after defeating an enemy, allows you to discover a clue at your location"
      $ do
          let rolandBanks = lookupInvestigator "01001"
          enemy <- testEnemy
            $ \attrs -> attrs { enemyFight = 4, enemyHealth = Static 1 }
          location <- testLocation
            $ \attrs -> attrs { locationClues = 1 }
          gameTest
              rolandBanks
              [ SetTokens [Zero]
              , enemySpawn location enemy
              , moveTo rolandBanks location
              , fightEnemy rolandBanks enemy
              ]
              ((enemiesL %~ insertEntity enemy)
              . (locationsL %~ insertEntity location)
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
                rolandBanks' <- updated rolandBanks
                getCount rolandBanks' `shouldReturn` ClueCount 1

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
          (locationsL %~ insertEntity location)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            rolandBanks' <- updated rolandBanks
            getCount rolandBanks' `shouldReturn` ClueCount 1
