module Arkham.Types.Event.Cards.EmergencyCacheSpec where

import TestImport

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" $ do
      investigator <- testInvestigator "00000" id
      emergencyCache <- buildEvent "01088" investigator
      game <- runGameTest
        investigator
        [playEvent investigator emergencyCache]
        (eventsL %~ insertEntity emergencyCache)
      updatedResourceCount game investigator `shouldBe` 3
