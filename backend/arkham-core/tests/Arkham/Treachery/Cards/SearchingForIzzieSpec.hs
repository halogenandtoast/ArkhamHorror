module Arkham.Treachery.Cards.SearchingForIzzieSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Treachery.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Searching for Izzie" $ do
  it "attaches to the location farthest away from you" $ do
    investigator <- testJenny id
    searchingForIzzie <- genPlayerCard Cards.searchingForIzzie
    (location1, location2) <- testConnectedLocations id id
    gameTest
        investigator
        [ placedLocation location1
        , placedLocation location2
        , loadDeck investigator [searchingForIzzie]
        , moveTo investigator location1
        , drawCards investigator 1
        ]
        ((entitiesL . locationsL %~ insertEntity location1)
        . (entitiesL . locationsL %~ insertEntity location2)
        )
      $ do
          runMessages
          selectAny
              (TreacheryAt (LocationWithId $ toId location2)
              <> treacheryIs Cards.searchingForIzzie
              )
            `shouldReturn` True

  it "takes 2 actions and is discarded on a successful investigation" $ do
    investigator <- testJenny id
    searchingForIzzie <- genPlayerCard Cards.searchingForIzzie
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , loadDeck investigator [searchingForIzzie]
        , moveTo investigator location
        , drawCards investigator 1
        , moveTo investigator location
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          searchingForIzzieId <- selectJust
            $ treacheryIs Cards.searchingForIzzie
          [searchingForIzzieAction, _] <- field
            TreacheryAbilities
            searchingForIzzieId
          push $ UseAbility (toId investigator) searchingForIzzieAction []
          runMessages
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"
          selectAny
              (TreacheryAt (LocationWithId $ toId location)
              <> treacheryIs Cards.searchingForIzzie
              )
            `shouldReturn` False
          fieldAssert InvestigatorDiscard (== [searchingForIzzie]) investigator
          fieldAssert InvestigatorRemainingActions (== 1) investigator
          fieldAssert InvestigatorMentalTrauma (== 0) investigator

  it "causes 1 mental trauma if not discarded" $ do
    investigator <- testJenny id
    searchingForIzzie <- genPlayerCard Cards.searchingForIzzie
    location <- testLocation id
    gameTest
        investigator
        [ loadDeck investigator [searchingForIzzie]
        , moveTo investigator location
        , drawCards investigator 1
        , EndOfGame Nothing
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOnlyOption "trigger searching for izzie"
          fieldAssert InvestigatorMentalTrauma (== 1) investigator
