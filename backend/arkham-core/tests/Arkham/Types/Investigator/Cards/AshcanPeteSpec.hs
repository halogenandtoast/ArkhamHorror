module Arkham.Types.Investigator.Cards.AshcanPeteSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "\"Ashcan\" Pete" $ do
  it "starts with Duke in play" $ do
    let ashcanPete = lookupInvestigator "02005"
    duke <- buildPlayerCard "02014"
    placeholders <- replicateM 5 (buildPlayerCard "01088") -- need to fill deck for setup

    game <- runGameTest
      ashcanPete
      [loadDeck ashcanPete (duke : placeholders), SetupInvestigators]
      id
    updated game ashcanPete `shouldSatisfy` hasCardInPlay (PlayerCard duke)
