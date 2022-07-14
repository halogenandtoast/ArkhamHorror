module Arkham.Event.Cards.CrypticResearch4Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Projection

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ do
      investigator <- testJenny id
      cards <- testPlayerCards 3
      location <- testLocation id
      crypticResearch4 <- buildEvent Events.crypticResearch4 investigator
      gameTest
          investigator
          [ loadDeck investigator cards
          , moveTo investigator location
          , playEvent investigator crypticResearch4
          ]
          ((entitiesL . eventsL %~ insertEntity crypticResearch4)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "choose self"

            isInDiscardOf investigator crypticResearch4 `shouldReturn` True
            field InvestigatorHand (toId investigator)
              `shouldMatchListM` map PlayerCard cards

    it "can select any investigator at the same location" $ do
      investigator <- testJenny id
      investigator2 <- testInvestigator Investigators.rolandBanks id
      cards <- testPlayerCards 3
      location <- testLocation id
      crypticResearch4 <- buildEvent Events.crypticResearch4 investigator
      gameTest
          investigator
          [ loadDeck investigator2 cards
          , moveAllTo location
          , playEvent investigator crypticResearch4
          ]
          ((entitiesL . eventsL %~ insertEntity crypticResearch4)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOptionMatching
              "choose other investigator"
              (\case
                TargetLabel (InvestigatorTarget iid') _
                  | iid' == toId investigator2 -> True
                _ -> False
              )
            isInDiscardOf investigator crypticResearch4 `shouldReturn` True
            field InvestigatorHand (toId investigator2)
              `shouldMatchListM` map PlayerCard cards
