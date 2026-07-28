module Arkham.Scenario.Scenarios.FateOfTheValeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Scenario.Scenarios.FateOfTheVale (crossOutUncontrolledResidents)
import TestImport.New

{- | Resolutions 1, 4 and 5 cross out "the name of each resident that was not
under control of an investigator at the end of the game". Resolution 5 in
particular is only reachable once every undefeated investigator has resigned —
and resigning eliminates the investigator, which discards the story assets they
control. Residents who escaped alongside a resigning investigator must still
count as controlled (see issue #5279, where every resident was crossed out even
though River Hawthorne had been rescued).
-}
spec :: Spec
spec = describe "Fate of the Vale" do
  describe "crossing out uncontrolled residents" do
    it "does not cross out a resident who resigned with an investigator" . scenarioTest "10651" $ \self -> do
      card <- genCard Assets.riverHawthorneBigInNewYork
      pushAndRun $ TakeControlOfSetAsideAsset (toId self) card
      pushAndRun $ Resign (toId self)
      runQueueT crossOutUncontrolledResidents
      runMessages
      getHasRecord RiverCrossedOut `shouldReturn` False

    it "crosses out a resident who was never under anyone's control" . scenarioTest "10651" $ \self -> do
      pushAndRun $ Resign (toId self)
      runQueueT crossOutUncontrolledResidents
      runMessages
      getHasRecord TheoCrossedOut `shouldReturn` True

    it "does not cross out a resident still in play at the end of the game" . scenarioTest "10651" $ \self -> do
      card <- genCard Assets.riverHawthorneBigInNewYork
      pushAndRun $ TakeControlOfSetAsideAsset (toId self) card
      runQueueT crossOutUncontrolledResidents
      runMessages
      getHasRecord RiverCrossedOut `shouldReturn` False
