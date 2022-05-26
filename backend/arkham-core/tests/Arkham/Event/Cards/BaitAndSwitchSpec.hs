module Arkham.Event.Cards.BaitAndSwitchSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Enemy.Attrs qualified as EnemyAttrs
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Matcher

spec :: Spec
spec = describe "Bait and Switch" $ do
  it "will move the enemy to a connected location if you succeed" $ do
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorAgility = 3 }
    enemy <- testEnemy (EnemyAttrs.evadeL .~ 3)
    baitAndSwitch <- buildEvent "02034" investigator
    (location1, location2) <- testConnectedLocations id id
    gameTest
        investigator
        [ placedLocation location1
        , placedLocation location2
        , SetTokens [Zero]
        , enemySpawn location1 enemy
        , moveTo investigator location1
        , playEvent investigator baitAndSwitch
        ]
        ((entitiesL . eventsL %~ insertEntity baitAndSwitch)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location1)
        . (entitiesL . locationsL %~ insertEntity location2)
        )
      $ do
          runMessages
          chooseOnlyOption "Evade enemy"
          chooseOnlyOption "Run skill check"
          chooseOnlyOption "Apply results"
          chooseOnlyOption "Move enemy"

          isInDiscardOf investigator baitAndSwitch `shouldReturn` True
          evadedBy investigator enemy `shouldReturn` True
          enemyLocation <- withGame $ selectJust $ LocationWithEnemy $ EnemyWithId (toId enemy)
          enemyLocation `shouldBe` toId location2
