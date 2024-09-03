module Arkham.Event.Cards.SearchForTheTruthSpec (
  spec,
) where

import TestImport

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Projection
import Arkham.Token

spec :: Spec
spec = describe "Search for the Truth" $ do
  it "allows you to draw cards equal to the number of clues" $ gameTest $ \investigator -> do
    updateInvestigator investigator (Investigator.tokensL %~ setTokens Clue 3)
    playerCards <- testPlayerCards 3
    pushAndRun $ loadDeck investigator playerCards
    putCardIntoPlay investigator Events.searchForTheTruth
    field InvestigatorHand (toId investigator)
      `shouldMatchListM` map PlayerCard playerCards

  it "has a maximum of 5 cards" $ gameTest $ \investigator -> do
    updateInvestigator investigator (Investigator.tokensL %~ setTokens Clue 6)
    playerCards <- testPlayerCards 6
    pushAndRun $ loadDeck investigator playerCards
    putCardIntoPlay investigator Events.searchForTheTruth
    fieldAssertLength InvestigatorHand 5 investigator
