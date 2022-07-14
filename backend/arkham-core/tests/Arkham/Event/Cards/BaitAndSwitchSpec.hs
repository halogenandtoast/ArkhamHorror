{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Event.Cards.BaitAndSwitchSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Enemy.Attrs qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Attrs (Field(..), InvestigatorAttrs(..))
import Arkham.Enemy.Attrs (Field(..))
import Arkham.Projection

spec :: Spec
spec = describe "Bait and Switch" $ do
  it "will move the enemy to a connected location if you succeed" $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorAgility = 3 }
    enemy <- testEnemy (EnemyAttrs.evadeL .~ 3)
    baitAndSwitch <- buildEvent Events.baitAndSwitch investigator
    let Just baitAndSwitchCard = preview _PlayerCard (toCard $ toAttrs baitAndSwitch)
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

          assert $ fieldP InvestigatorDiscard (== [baitAndSwitchCard]) (toId investigator)
          assert $ fieldP EnemyEngagedInvestigators null (toId enemy)
          assert $ fieldP EnemyLocation (== Just (toId location2)) (toId enemy)
