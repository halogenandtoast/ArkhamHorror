module Arkham.Types.Event.Cards.BaitAndSwitchSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = describe "Bait and Switch" $ do
  it "will move the enemy to a connected location if you succeed" $ do
    scenario' <- testScenario "00000" id
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorAgility = 3 }
    enemy <- testEnemy (set EnemyAttrs.evade 3)
    baitAndSwitch <- buildEvent "02034" investigator
    (location1, location2) <- testConnectedLocations id id
    game <-
      runGameTest
        investigator
        [ PlacedLocation (getId () location1)
        , PlacedLocation (getId () location2)
        , SetTokens [Zero]
        , enemySpawn location1 enemy
        , moveTo investigator location1
        , playEvent investigator baitAndSwitch
        ]
        ((events %~ insertEntity baitAndSwitch)
        . (enemies %~ insertEntity enemy)
        . (locations %~ insertEntity location1)
        . (locations %~ insertEntity location2)
        . (scenario ?~ scenario')
        )
      >>= runGameTestOnlyOption "Evade enemy"
      >>= runGameTestOnlyOption "Run skill check"
      >>= runGameTestOnlyOption "Apply results"
      >>= runGameTestOnlyOption "Move enemy"
    baitAndSwitch `shouldSatisfy` isInDiscardOf game investigator
    enemy `shouldSatisfy` evadedBy game investigator
    enemyLocation <- withGame
      game
      (asks (getId @LocationId @EnemyId (getId () enemy)))
    enemyLocation `shouldBe` getId () location2
