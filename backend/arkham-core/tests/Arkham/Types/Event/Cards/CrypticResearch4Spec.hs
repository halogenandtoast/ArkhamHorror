module Arkham.Types.Event.Cards.CrypticResearch4Spec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ do
      (investigatorId, investigator) <- testInvestigator "00000" id
      (locationId, location) <- testLocation "00000" id
      (crypticResearch4Id, crypticResearch4) <- buildEvent
        "01043"
        investigatorId
      game <-
        runGameTest
            investigator
            [ MoveTo investigatorId locationId
            , InvestigatorPlayEvent investigatorId crypticResearch4Id
            ]
            ((events %~ insertMap crypticResearch4Id crypticResearch4)
            . (locations %~ insertMap locationId location)
            )
          >>= runGameTestOnlyOption "choose self"
      crypticResearch4 `shouldSatisfy` isInDiscardOf game investigator
