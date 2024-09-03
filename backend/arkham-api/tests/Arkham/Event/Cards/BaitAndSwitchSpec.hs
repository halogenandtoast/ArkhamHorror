module Arkham.Event.Cards.BaitAndSwitchSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import Arkham.Projection

spec :: Spec
spec = describe "Bait and Switch" $ do
  it "will move the enemy to a connected location if you succeed" $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ \attrs -> attrs {investigatorAgility = 3}
    enemy <- testEnemyWith (EnemyAttrs.evadeL ?~ 3)
    (location1, location2) <- testConnectedLocations id id
    pushAndRunAll
      [ placedLocation location1
      , placedLocation location2
      , SetChaosTokens [Zero]
      , spawnAt enemy location1
      , moveTo investigator location1
      ]

    putCardIntoPlay investigator Events.baitAndSwitch
    chooseOnlyOption "Evade enemy"
    chooseOnlyOption "Run skill check"
    chooseOnlyOption "Apply results"
    chooseOnlyOption "Move enemy"

    assert $ Events.baitAndSwitch `isInDiscardOf` investigator
    assert $ fieldP EnemyEngagedInvestigators null (toId enemy)
    assert $ fieldP EnemyLocation (== Just (toId location2)) (toId enemy)
