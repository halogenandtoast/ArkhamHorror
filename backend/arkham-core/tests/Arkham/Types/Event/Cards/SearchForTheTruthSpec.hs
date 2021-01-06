module Arkham.Types.Event.Cards.SearchForTheTruthSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Investigator.Attrs as Investigator

spec :: Spec
spec = describe "Search for the Truth" $ do
  it "allows you to draw cards equal to the number of clues" $ do
    investigator <- testInvestigator "00000" (Investigator.cluesL .~ 3)
    playerCards <- testPlayerCards 3
    searchForTheTruth <- buildEvent "02008" investigator
    game <- runGameTest
      investigator
      [ loadDeck investigator playerCards
      , playEvent investigator searchForTheTruth
      ]
      (eventsL %~ insertEntity searchForTheTruth)
    updated game investigator
      `shouldSatisfy` handIs (map PlayerCard playerCards)

  it "has a maximum of 5 cards" $ do
    investigator <- testInvestigator "00000" (Investigator.cluesL .~ 6)
    playerCards <- testPlayerCards 6
    searchForTheTruth <- buildEvent "02008" investigator
    game <- runGameTest
      investigator
      [ loadDeck investigator playerCards
      , playEvent investigator searchForTheTruth
      ]
      (eventsL %~ insertEntity searchForTheTruth)
    updated game investigator `shouldSatisfy` handMatches ((== 5) . length)
