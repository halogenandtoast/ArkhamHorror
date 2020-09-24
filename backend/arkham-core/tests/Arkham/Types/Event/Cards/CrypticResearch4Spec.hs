module Arkham.Types.Event.Cards.CrypticResearch4Spec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ do
      investigator <- testInvestigator "00000" id
      location <- testLocation "00000" id
      crypticResearch4 <- buildEvent "01043" investigator
      game <-
        runGameTest
            investigator
            [ moveTo investigator location
            , playEvent investigator crypticResearch4
            ]
            ((events %~ insertEntity crypticResearch4)
            . (locations %~ insertEntity location)
            )
          >>= runGameTestOnlyOption "choose self"
      crypticResearch4 `shouldSatisfy` isInDiscardOf game investigator
