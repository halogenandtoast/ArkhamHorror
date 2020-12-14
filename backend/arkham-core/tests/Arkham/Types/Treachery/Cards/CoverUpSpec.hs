module Arkham.Types.Treachery.Cards.CoverUpSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "Cover Up" $ do
  it "starts with 3 clues on it" $ do
    investigator <- testInvestigator "00000" id
    coverUp <- buildPlayerCard "01007"
    game <- runGameTest
      investigator
      [loadDeck investigator [coverUp], drawCards investigator 1]
      id
    let coverUpTreachery = game ^?! treacheries . to toList . ix 0
    withGame game (getCount coverUpTreachery) `shouldReturn` Just (ClueCount 3)
