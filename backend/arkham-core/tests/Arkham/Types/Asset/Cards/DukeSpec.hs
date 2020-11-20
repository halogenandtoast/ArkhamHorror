module Arkham.Types.Asset.Cards.DukeSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Location.Attrs (Attrs(..))

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage" $ do
      duke <- buildAsset "02014"
      enemy <- testEnemy ((Enemy.health .~ Static 3) . (Enemy.fight .~ 4))
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorCombat = 1 }
      location <- testLocation "00000" id
      scenario' <- testScenario "00000" id
      game <- runGameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator duke
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((enemies %~ insertEntity enemy)
        . (locations %~ insertEntity location)
        . (scenario ?~ scenario')
        . (assets %~ insertEntity duke)
        )
      let dukeAsset = game ^?! assets . to toList . ix 0
      [fightAction, _] <- getActionsOf game investigator NonFast dukeAsset
      game' <-
        runGameTestMessages game [fightAction]
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Start skill test"
        >>= runGameTestOnlyOption "Apply Results"
      updated game' enemy `shouldSatisfy` hasDamage (2, 0)
  context "investigate action" $ do
    it "uses a base intellect skill of 4" $ do
      duke <- buildAsset "02014"
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorIntellect = 1 }
      location <- testLocation
        "00000"
        (\attrs -> attrs { locationShroud = 4, locationClues = 1 })
      scenario' <- testScenario "00000" id
      game <- runGameTest
        investigator
        [SetTokens [Zero], playAsset investigator duke]
        ((locations %~ insertEntity location)
        . (scenario ?~ scenario')
        . (assets %~ insertEntity duke)
        )
      let dukeAsset = game ^?! assets . to toList . ix 0
      [investigateAction] <- getActionsOf game investigator NonFast dukeAsset
      game' <-
        runGameTestMessages
          game
          [moveTo investigator location, investigateAction]
        >>= runGameTestOnlyOption "Start skill test"
        >>= runGameTestOnlyOption "Apply results"
      updated game' investigator `shouldSatisfy` hasClueCount 1
    it "you may move to a connecting location immediately before investigating"
      $ do
          duke <- buildAsset "02014"
          investigator <- testInvestigator "00000"
            $ \attrs -> attrs { investigatorIntellect = 1 }
          (location1, location2) <- testConnectedLocations id
            $ \attrs -> attrs { locationShroud = 4, locationClues = 1 }
          scenario' <- testScenario "00000" id
          game <- runGameTest
            investigator
            [ PlacedLocation (getLocationId location1)
            , PlacedLocation (getLocationId location2)
            , SetTokens [Zero]
            , playAsset investigator duke
            ]
            ((locations %~ insertEntity location1)
            . (locations %~ insertEntity location2)
            . (scenario ?~ scenario')
            . (assets %~ insertEntity duke)
            )
          let dukeAsset = game ^?! assets . to toList . ix 0
          [investigateAction] <- toInternalGame game >>= runReaderT
            (getActions (getInvestigatorId investigator) NonFast dukeAsset)
          game' <-
            runGameTestMessages
              game
              [moveTo investigator location1, investigateAction]
            >>= runGameTestOptionMatching
                  "move first"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
            >>= runGameTestOnlyOption "Start skill test"
            >>= runGameTestOnlyOption "Apply results"
          updated game' investigator `shouldSatisfy` hasClueCount 1
