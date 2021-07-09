module Arkham.Types.Asset.Cards.DukeSpec
  ( spec
  ) where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Types.Location.Attrs (LocationAttrs(..))

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage" $ do
      duke <- buildAsset "02014"
      enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 4))
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorCombat = 1 }
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator duke
          , enemySpawn location enemy
          , moveTo investigator location
          ]
          ((enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          . (assetsL %~ insertEntity duke)
          )
        $ do
            runMessages
            duke' <- updated duke
            [doFight, _] <- getActionsOf investigator NonFast duke'
            push doFight
            runMessages
            chooseOnlyOption "Fight enemy"
            chooseOnlyOption "Start skill test"
            chooseOnlyOption "Apply Results"
            updated enemy `shouldSatisfyM` hasDamage (2, 0)
  context "investigate action" $ do
    it "uses a base intellect skill of 4" $ do
      duke <- buildAsset "02014"
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorIntellect = 1 }
      location <- testLocation
        (\attrs -> attrs { locationShroud = 4, locationClues = 1 })
      gameTest
          investigator
          [ SetTokens [Zero]
          , moveTo investigator location
          , playAsset investigator duke
          ]
          ((locationsL %~ insertEntity location)
          . (assetsL %~ insertEntity duke)
          )
        $ do
            runMessages
            duke' <- updated duke
            [_, investigateAction] <- getActionsOf investigator NonFast duke'
            pushAll [moveTo investigator location, investigateAction]
            runMessages
            chooseOnlyOption "Start skill test"
            chooseOnlyOption "Apply results"
            updated investigator `shouldSatisfyM` hasClueCount 1
    it "you may move to a connecting location immediately before investigating"
      $ do
          duke <- buildAsset "02014"
          investigator <- testInvestigator "00000"
            $ \attrs -> attrs { investigatorIntellect = 1 }
          (location1, location2) <- testConnectedLocations id
            $ \attrs -> attrs { locationShroud = 4, locationClues = 1 }
          gameTest
              investigator
              [ placedLocation location1
              , placedLocation location2
              , SetTokens [Zero]
              , moveTo investigator location1
              , playAsset investigator duke
              ]
              ((locationsL %~ insertEntity location1)
              . (locationsL %~ insertEntity location2)
              . (assetsL %~ insertEntity duke)
              )
            $ do
                runMessages
                duke' <- updated duke
                [_, investigateAction] <- getActionsOf
                  investigator
                  NonFast
                  duke'
                pushAll [moveTo investigator location1, investigateAction]
                runMessages
                chooseOptionMatching
                  "move first"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
                chooseOnlyOption "Start skill test"
                chooseOnlyOption "Apply results"
                updated investigator `shouldSatisfyM` hasClueCount 1
