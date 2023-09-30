module Arkham.Treachery.Cards.SearchingForIzzieSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Types (Field (..))

spec :: Spec
spec = describe "Searching for Izzie" $ do
  it "attaches to the location farthest away from you" $ gameTest $ \investigator -> do
    searchingForIzzie <- genPlayerCard Cards.searchingForIzzie
    drawing <- drawCards (toId investigator) investigator 1
    (location1, location2) <- testConnectedLocations id id
    pushAndRunAll
      [ placedLocation location1
      , placedLocation location2
      , loadDeck investigator [searchingForIzzie]
      , moveTo investigator location1
      , drawing
      ]
    assert $
      selectAny $
        TreacheryAt (LocationWithId $ toId location2)
          <> treacheryIs Cards.searchingForIzzie

  it "takes 2 actions and is discarded on a successful investigation" $ gameTest $ \investigator -> do
    searchingForIzzie <- genPlayerCard Cards.searchingForIzzie
    location <- testLocationWith id
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ SetChaosTokens [Zero]
      , loadDeck investigator [searchingForIzzie]
      , moveTo investigator location
      , drawing
      , moveTo investigator location
      ]
    searchingForIzzieId <-
      selectJust $
        treacheryIs Cards.searchingForIzzie
    [searchingForIzzieAction, _] <-
      field
        TreacheryAbilities
        searchingForIzzieId
    pushAndRun $ UseAbility (toId investigator) searchingForIzzieAction []
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"
    assert $
      selectNone $
        treacheryAt (toId location)
          <> treacheryIs Cards.searchingForIzzie
    fieldAssert InvestigatorDiscard (== [searchingForIzzie]) investigator
    fieldAssert InvestigatorRemainingActions (== 1) investigator
    fieldAssert InvestigatorMentalTrauma (== 0) investigator

  it "causes 1 mental trauma if not discarded" $ gameTest $ \investigator -> do
    searchingForIzzie <- genPlayerCard Cards.searchingForIzzie
    location <- testLocationWith id
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ loadDeck investigator [searchingForIzzie]
      , moveTo investigator location
      , drawing
      , EndOfGame Nothing
      ]
    chooseOnlyOption "trigger searching for izzie"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator
