module Arkham.Treachery.Cards.CoverUpSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Location.Types qualified as Location
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Types (Field (..))

spec :: Spec
spec = describe "Cover Up" $ do
  it "starts with 3 clues on it" $ gameTest $ \investigator -> do
    coverUp <- genPlayerCard Cards.coverUp
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll [loadDeck investigator [coverUp], drawing]
    coverUpId <- selectJust $ treacheryIs Cards.coverUp
    field TreacheryClues coverUpId `shouldReturn` 3

  it "allows you to remove a clue instead of discovering clues" $ gameTest $ \investigator -> do
    coverUp <- genPlayerCard Cards.coverUp
    location <- testLocation $ Location.revealCluesL .~ Static 1
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ loadDeck investigator [coverUp]
      , drawing
      , moveTo investigator location
      , InvestigatorDiscoverClues (toId investigator) (toId location) GameSource 1 Nothing
      ]
    chooseOptionMatching
      "Use ability"
      ( \case
          AbilityLabel {} -> True
          _ -> False
      )
    coverUpId <- selectJust $ treacheryIs Cards.coverUp
    field TreacheryClues coverUpId `shouldReturn` 2
    fieldAssert LocationClues (== 1) location

  it "causes one mental trauma when the game ends if there are any clues on it" $ gameTest $ \investigator -> do
    coverUp <- genPlayerCard Cards.coverUp
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ loadDeck investigator [coverUp]
      , drawing
      , EndOfGame Nothing
      ]
    chooseOnlyOption "trigger cover up"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator

  it "does not cause trauma when the game ends if there are no clues on it" $ gameTest $ \investigator -> do
    coverUp <- genPlayerCard Cards.coverUp
    location <- testLocation $ Location.revealCluesL .~ Static 3
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ loadDeck investigator [coverUp]
      , drawing
      , moveTo investigator location
      , InvestigatorDiscoverClues (toId investigator) (toId location) GameSource 3 Nothing
      , EndOfGame Nothing
      ]
    chooseOptionMatching
      "Use ability"
      ( \case
          AbilityLabel {} -> True
          _ -> False
      )
    coverUpId <- selectJust $ treacheryIs Cards.coverUp
    field TreacheryClues coverUpId `shouldReturn` 0
    fieldAssert InvestigatorMentalTrauma (== 0) investigator
