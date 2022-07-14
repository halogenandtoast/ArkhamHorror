module Arkham.Event.Cards.SearchForTheTruthSpec
  ( spec
  ) where

import TestImport

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Investigator.Attrs qualified as Investigator
import Arkham.Projection

spec :: Spec
spec = describe "Search for the Truth" $ do
  it "allows you to draw cards equal to the number of clues" $ do
    investigator <- testJenny (Investigator.cluesL .~ 3)
    playerCards <- testPlayerCards 3
    searchForTheTruth <- buildEvent Events.searchForTheTruth investigator
    gameTest
        investigator
        [ loadDeck investigator playerCards
        , playEvent investigator searchForTheTruth
        ]
        (entitiesL . eventsL %~ insertEntity searchForTheTruth)
      $ do
          runMessages
          field InvestigatorHand (toId investigator)
            `shouldMatchListM` map PlayerCard playerCards

  it "has a maximum of 5 cards" $ do
    investigator <- testJenny (Investigator.cluesL .~ 6)
    playerCards <- testPlayerCards 6
    searchForTheTruth <- buildEvent Events.searchForTheTruth investigator
    gameTest
        investigator
        [ loadDeck investigator playerCards
        , playEvent investigator searchForTheTruth
        ]
        (entitiesL . eventsL %~ insertEntity searchForTheTruth)
      $ do
          runMessages
          fieldAssert InvestigatorHand ((== 5) . length) investigator
