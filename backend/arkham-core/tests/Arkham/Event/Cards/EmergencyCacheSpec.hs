module Arkham.Event.Cards.EmergencyCacheSpec where

import TestImport.Lifted

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" $ do
      investigator <- testInvestigator "00000" id
      emergencyCache <- buildEvent "01088" investigator
      gameTest
          investigator
          [playEvent investigator emergencyCache]
          (eventsL %~ insertEntity emergencyCache)
        $ do
            runMessages
            updatedResourceCount investigator `shouldReturn` 3
