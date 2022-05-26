module Arkham.Asset.Cards.DukeSpec
  ( spec
  ) where

import TestImport

import Arkham.Enemy.Attrs qualified as Enemy
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Location.Attrs (LocationAttrs(..))

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage" $ do
      duke <- buildAsset "02014"
      enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 4))
      investigator <- testInvestigator
        $ \attrs -> attrs { investigatorCombat = 1 }
      location <- testLocation id
      gameTest
          investigator
          [ SetTokens [Zero]
          , playAsset investigator duke
          , enemySpawn location enemy
          , moveTo investigator location
          ]
          ((entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . assetsL %~ insertEntity duke)
          )
        $ do
            runMessages
            duke' <- updated duke
            [doFight, _] <- getAbilitiesOf duke'
            push $ UseAbility (toId investigator) doFight []
            runMessages
            chooseOnlyOption "Fight enemy"
            chooseOnlyOption "Start skill test"
            chooseOnlyOption "Apply Results"
            updated enemy `shouldSatisfyM` hasDamage (2, 0)
  context "investigate action" $ do
    it "uses a base intellect skill of 4" $ do
      duke <- buildAsset "02014"
      investigator <- testInvestigator
        $ \attrs -> attrs { investigatorIntellect = 1 }
      location <- testLocation
        (\attrs -> attrs { locationShroud = 4, locationClues = 1 })
      gameTest
          investigator
          [ SetTokens [Zero]
          , moveTo investigator location
          , playAsset investigator duke
          ]
          ((entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . assetsL %~ insertEntity duke)
          )
        $ do
            runMessages
            duke' <- updated duke
            [_, investigateAction] <- getAbilitiesOf duke'
            push $ UseAbility (toId investigator) investigateAction []
            runMessages
            chooseOnlyOption "Start skill test"
            chooseOnlyOption "Apply results"
            updated investigator `shouldSatisfyM` hasClueCount 1
    it "you may move to a connecting location immediately before investigating"
      $ do
          duke <- buildAsset "02014"
          investigator <- testInvestigator
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
              ((entitiesL . locationsL %~ insertEntity location1)
              . (entitiesL . locationsL %~ insertEntity location2)
              . (entitiesL . assetsL %~ insertEntity duke)
              )
            $ do
                runMessages
                duke' <- updated duke
                [_, investigateAction] <- getAbilitiesOf duke'
                pushAll
                  [ moveTo investigator location1
                  , UseAbility (toId investigator) investigateAction []
                  ]
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
