module Arkham.Types.Event.Cards.CrypticResearch4Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Target

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ do
      investigator <- testInvestigator "00000" id
      cards <- testPlayerCards 3
      location <- testLocation "00000" id
      crypticResearch4 <- buildEvent "01043" investigator
      game <-
        runGameTest
            investigator
            [ loadDeck investigator cards
            , moveTo investigator location
            , playEvent investigator crypticResearch4
            ]
            ((events %~ insertEntity crypticResearch4)
            . (locations %~ insertEntity location)
            )
          >>= runGameTestOnlyOption "choose self"
      crypticResearch4 `shouldSatisfy` isInDiscardOf game investigator
      investigator `shouldSatisfy` handIs game (map PlayerCard cards)

    it "can select any investigator at the same location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      cards <- testPlayerCards 3
      location <- testLocation "00000" id
      crypticResearch4 <- buildEvent "01043" investigator
      game <-
        runGameTest
            investigator
            [ loadDeck investigator2 cards
            , moveAllTo location
            , playEvent investigator crypticResearch4
            ]
            ((events %~ insertEntity crypticResearch4)
            . (locations %~ insertEntity location)
            . (investigators %~ insertEntity investigator2)
            )
          >>= runGameTestOptionMatching
                "choose other investigator"
                (\case
                  TargetLabel (InvestigatorTarget "00001") _ -> True
                  _ -> False
                )
      crypticResearch4 `shouldSatisfy` isInDiscardOf game investigator
      investigator2 `shouldSatisfy` handIs game (map PlayerCard cards)
