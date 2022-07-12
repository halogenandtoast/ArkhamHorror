module Arkham.Asset.Cards.DukeSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Enemy.Attrs qualified as Enemy
import Arkham.Investigator.Attrs (Field (..), InvestigatorAttrs(..))
import Arkham.Asset.Attrs (Field (..) )
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Attrs (Field (..) )
import Arkham.Location.Attrs (LocationAttrs(..))
import Arkham.Projection
import Arkham.Timing qualified as Timing

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage" $ do
      enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 4))
      investigator <- testInvestigator
        $ \attrs -> attrs { investigatorCombat = 1 }
      duke <- buildAsset Assets.duke (Just investigator)
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
            [doFight, _] <- field AssetAbilities (toId duke)
            push $ UseAbility (toId investigator) doFight []
            runMessages
            chooseOnlyOption "Fight enemy"
            chooseOnlyOption "Start skill test"
            chooseOnlyOption "Apply Results"
            fieldAssert EnemyDamage (== 2) enemy
  context "investigate action" $ do
    it "uses a base intellect skill of 4" $ do
      investigator <- testInvestigator
        $ \attrs -> attrs { investigatorIntellect = 1 }
      duke <- buildAsset Assets.duke (Just investigator)
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
            [_, investigateAction] <- field AssetAbilities (toId duke)
            push $ UseAbility (toId investigator) investigateAction [Window Timing.When (DuringTurn $ toId investigator)]
            runMessages
            chooseOnlyOption "Start skill test"
            chooseOnlyOption "Apply results"
            fieldAssert InvestigatorClues (== 1) investigator
    it "you may move to a connecting location immediately before investigating"
      $ do
          investigator <- testInvestigator
            $ \attrs -> attrs { investigatorIntellect = 1 }
          duke <- buildAsset Assets.duke (Just investigator)
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
                [_, investigateAction] <- field AssetAbilities (toId duke)
                pushAll
                  [ moveTo investigator location1
                  , UseAbility (toId investigator) investigateAction []
                  ]
                runMessages
                chooseOptionMatching
                  "move first"
                  (\case
                    TargetLabel{} -> True
                    _ -> False
                  )
                chooseOnlyOption "Start skill test"
                chooseOnlyOption "Apply results"
                fieldAssert InvestigatorClues (== 1) investigator
