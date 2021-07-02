module Arkham.Types.Treachery.Cards.CoverUpSpec
  ( spec
  )
where

import TestImport.Lifted

import qualified Arkham.Types.Location.Attrs as Location

spec :: Spec
spec = describe "Cover Up" $ do
  it "starts with 3 clues on it" $ do
    investigator <- testInvestigator "00000" id
    coverUp <- buildPlayerCard "01007"
    gameTest
        investigator
        [loadDeck investigator [coverUp], drawCards investigator 1]
        id
      $ do
          runMessages
          game <- getTestGame
          let coverUpTreachery = game ^?! treacheriesL . to toList . ix 0
          getCount coverUpTreachery `shouldReturn` Just (ClueCount 3)

  it "allows you to remove a clue instead of discovering clues" $ do
    investigator <- testInvestigator "00000" id
    coverUp <- buildPlayerCard "01007"
    location <- testLocation $ Location.cluesL .~ 1
    gameTest
        investigator
        [ loadDeck investigator [coverUp]
        , drawCards investigator 1
        , moveTo investigator location
        , DiscoverCluesAtLocation (toId investigator) (toId location) 1 Nothing
        ]
        (locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOptionMatching
            "Use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          game <- getTestGame
          let coverUpTreachery = game ^?! treacheriesL . to toList . ix 0
          getCount coverUpTreachery `shouldReturn` Just (ClueCount 2)
          getCount location `shouldReturn` ClueCount 1

  it "causes one mental trauma when the game ends if there are any clues on it"
    $ do
        investigator <- testInvestigator "00000" id
        coverUp <- buildPlayerCard "01007"
        gameTest
            investigator
            [ loadDeck investigator [coverUp]
            , drawCards investigator 1
            , EndOfGame
            ]
            id
          $ do
              runMessages
              getCount (toId investigator) `shouldReturn` MentalTraumaCount 1

  it "does not cause trauma when the game ends if there are no clues on it" $ do
    investigator <- testInvestigator "00000" id
    coverUp <- buildPlayerCard "01007"
    location <- testLocation $ Location.cluesL .~ 3
    gameTest
        investigator
        [ loadDeck investigator [coverUp]
        , drawCards investigator 1
        , moveTo investigator location
        , DiscoverCluesAtLocation (toId investigator) (toId location) 3 Nothing
        , EndOfGame
        ]
        (locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOptionMatching
            "Use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          game <- getTestGame
          let coverUpTreachery = game ^?! treacheriesL . to toList . ix 0
          getCount (toId coverUpTreachery) `shouldReturn` Just (ClueCount 0)
          getCount (toId investigator) `shouldReturn` MentalTraumaCount 0
