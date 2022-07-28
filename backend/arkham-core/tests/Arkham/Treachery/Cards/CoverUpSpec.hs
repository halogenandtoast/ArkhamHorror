module Arkham.Treachery.Cards.CoverUpSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Location.Types qualified as Location
import Arkham.Location.Types (Field(..))
import Arkham.Investigator.Types (Field(..))
import Arkham.Treachery.Types (Field(..))
import Arkham.Projection
import Arkham.Matcher

spec :: Spec
spec = describe "Cover Up" $ do
  it "starts with 3 clues on it" $ do
    investigator <- testJenny id
    coverUp <- genPlayerCard Cards.coverUp
    gameTest
        investigator
        [loadDeck investigator [coverUp], drawCards investigator 1]
        id
      $ do
          runMessages
          coverUpId <- selectJust $ treacheryIs Cards.coverUp
          field TreacheryClues coverUpId `shouldReturn` 3

  it "allows you to remove a clue instead of discovering clues" $ do
    investigator <- testJenny id
    coverUp <- genPlayerCard Cards.coverUp
    location <- testLocation $ Location.cluesL .~ 1
    gameTest
        investigator
        [ loadDeck investigator [coverUp]
        , drawCards investigator 1
        , moveTo investigator location
        , InvestigatorDiscoverClues (toId investigator) (toId location) 1 Nothing
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOptionMatching
            "Use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          coverUpId <- selectJust $ treacheryIs Cards.coverUp
          field TreacheryClues coverUpId `shouldReturn` 2
          fieldAssert LocationClues (== 1) location

  it "causes one mental trauma when the game ends if there are any clues on it"
    $ do
        investigator <- testJenny id
        coverUp <- genPlayerCard Cards.coverUp
        gameTest
            investigator
            [ loadDeck investigator [coverUp]
            , drawCards investigator 1
            , EndOfGame Nothing
            ]
            id
          $ do
              runMessages
              chooseOnlyOption "trigger cover up"
              fieldAssert InvestigatorMentalTrauma (== 1) investigator

  it "does not cause trauma when the game ends if there are no clues on it" $ do
    investigator <- testJenny id
    coverUp <- genPlayerCard Cards.coverUp
    location <- testLocation $ Location.cluesL .~ 3
    gameTest
        investigator
        [ loadDeck investigator [coverUp]
        , drawCards investigator 1
        , moveTo investigator location
        , InvestigatorDiscoverClues (toId investigator) (toId location) 3 Nothing
        , EndOfGame Nothing
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOptionMatching
            "Use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          coverUpId <- selectJust $ treacheryIs Cards.coverUp
          field TreacheryClues coverUpId `shouldReturn` 0
          fieldAssert InvestigatorMentalTrauma (== 0) investigator
