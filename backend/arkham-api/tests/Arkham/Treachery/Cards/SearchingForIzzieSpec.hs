module Arkham.Treachery.Cards.SearchingForIzzieSpec (spec) where

import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Types (Field (..))
import TestImport.New

spec :: Spec
spec = describe "Searching for Izzie" $ do
  it "attaches to the location farthest away from you" $ gameTest $ \investigator -> do
    (location1, location2) <- testConnectedLocations id id
    pushAndRunAll [placedLocation location1, placedLocation location2]
    loadDeck investigator [Cards.searchingForIzzie]
    moveTo investigator location1
    drawCards investigator 1
    assert
      $ selectAny
      $ TreacheryAt (LocationWithId $ toId location2)
      <> treacheryIs Cards.searchingForIzzie

  it "takes 2 actions and is discarded on a successful investigation" $ gameTest $ \investigator -> do
    location <- testLocationWith id
    setChaosTokens [Zero]
    loadDeck investigator [Cards.searchingForIzzie]
    moveTo investigator location
    drawCards investigator 1
    moveTo investigator location
    searchingForIzzieId <- selectJust $ treacheryIs Cards.searchingForIzzie
    [searchingForIzzieAction, _] <- field TreacheryAbilities searchingForIzzieId
    pushAndRun $ UseAbility (toId investigator) searchingForIzzieAction []
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"
    assert $ selectNone $ treacheryAt (toId location) <> treacheryIs Cards.searchingForIzzie
    fieldAssert InvestigatorDiscard (all ((== Cards.searchingForIzzie) . toCardDef)) investigator
    fieldAssert InvestigatorRemainingActions (== 1) investigator
    fieldAssert InvestigatorMentalTrauma (== 0) investigator

  it "causes 1 mental trauma if not discarded" $ gameTest $ \investigator -> do
    location <- testLocationWith id
    loadDeck investigator [Cards.searchingForIzzie]
    moveTo investigator location
    drawCards investigator 1
    run $ EndOfGame Nothing
    chooseOnlyOption "trigger searching for izzie"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator
