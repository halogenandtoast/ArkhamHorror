module Arkham.Types.Event.Cards.CrypticResearch4Spec
  ( spec
  )
where

import TestImport.Lifted

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ do
      investigator <- testInvestigator "00000" id
      cards <- testPlayerCards 3
      location <- testLocation id
      crypticResearch4 <- buildEvent "01043" investigator
      gameTest
          investigator
          [ loadDeck investigator cards
          , moveTo investigator location
          , playEvent investigator crypticResearch4
          ]
          ((eventsL %~ insertEntity crypticResearch4)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "choose self"

            isInDiscardOf investigator crypticResearch4 `shouldReturn` True
            updated investigator `shouldSatisfyM` handIs (map PlayerCard cards)

    it "can select any investigator at the same location" $ do
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      cards <- testPlayerCards 3
      location <- testLocation id
      crypticResearch4 <- buildEvent "01043" investigator
      gameTest
          investigator
          [ loadDeck investigator2 cards
          , moveAllTo location
          , playEvent investigator crypticResearch4
          ]
          ((eventsL %~ insertEntity crypticResearch4)
          . (locationsL %~ insertEntity location)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOptionMatching
              "choose other investigator"
              (\case
                TargetLabel (InvestigatorTarget "00001") _ -> True
                _ -> False
              )
            isInDiscardOf investigator crypticResearch4 `shouldReturn` True
            updated investigator2 `shouldSatisfyM` handIs (map PlayerCard cards)
