module Arkham.Types.Event.Cards.EmergencyCacheSpec where

import TestImport

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" $ do
      (investigatorId, investigator) <- testInvestigator "00000" id
      (eventId, event) <- buildEvent "01088" investigatorId
      game <- runGameTest
        investigator
        [InvestigatorPlayEvent investigatorId eventId]
        (events %~ insertMap eventId event)
      updatedResourceCount game investigator `shouldBe` 3
