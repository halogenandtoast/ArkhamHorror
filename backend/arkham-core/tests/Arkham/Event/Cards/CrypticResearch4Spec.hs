module Arkham.Event.Cards.CrypticResearch4Spec (
  spec,
) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ gameTest $ \investigator -> do
      cards <- testPlayerCards 3
      location <- testLocation id
      pushAndRunAll
        [ loadDeck investigator cards
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.crypticResearch4
      chooseOnlyOption "choose self"

      assert $ isInDiscardOf investigator Events.crypticResearch4
      field InvestigatorHand (toId investigator)
        `shouldMatchListM` map PlayerCard cards

    it "can select any investigator at the same location" $ gameTest $ \investigator -> do
      investigator2 <- addInvestigator Investigators.rolandBanks id
      cards <- testPlayerCards 3
      location <- testLocation id
      pushAndRunAll
        [ loadDeck investigator2 cards
        , moveAllTo location
        ]
      putCardIntoPlay investigator Events.crypticResearch4
      chooseOptionMatching
        "choose other investigator"
        ( \case
            TargetLabel (InvestigatorTarget iid') _
              | iid' == toId investigator2 -> True
            _ -> False
        )
      assert $ isInDiscardOf investigator Events.crypticResearch4
      field InvestigatorHand (toId investigator2)
        `shouldMatchListM` map PlayerCard cards
