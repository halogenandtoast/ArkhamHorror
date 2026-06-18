module Arkham.Event.Events.PredatorOrPreySpec (spec) where

import Arkham.Event.Cards qualified as Events
import Data.Text qualified as T
import TestImport.New

spec :: Spec
spec = describe "Predator or Prey" $ do
  it "re-engages the enemy when an engaged investigator chooses to flee but cannot move (#4845)" . gameTest $ \self -> do
    -- An isolated location: there is nowhere to move away to, so the
    -- "investigators move" option can disengage but not relocate the investigator.
    location <- testLocation & prop @"revealed" True
    self `moveTo` location
    enemy <- testEnemy
    enemy `spawnAt` location
    self.engagedEnemies `shouldReturn` [toId enemy]
    self `drawsCard` Events.predatorOrPrey
    chooseOptionMatching "investigators move" \case
      Label lbl _ -> "investigatorsMove" `T.isInfixOf` lbl
      _ -> False
    chooseTarget self
    -- The investigator could not move away, so the enemy must re-engage rather
    -- than be left stranded and unengaged at the investigator's location.
    self.engagedEnemies `shouldReturn` [toId enemy]
