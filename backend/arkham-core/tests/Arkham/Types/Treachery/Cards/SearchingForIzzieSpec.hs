module Arkham.Types.Treachery.Cards.SearchingForIzzieSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "Searching for Izzie" $ do
  it "attaches to the location farthest away from you" $ do
    investigator <- testInvestigator "00000" id
    searchingForIzzie <- buildPlayerCard "02011"
    (location1, location2) <- testConnectedLocations id id
    game <- runGameTest
      investigator
      [ placedLocation location1
      , placedLocation location2
      , loadDeck investigator [searchingForIzzie]
      , drawCards investigator 1
      ]
      ((locationsL %~ insertEntity location1)
      . (locationsL %~ insertEntity location2)
      )
    updated game location2 `shouldSatisfy` hasTreacheryWithMatchingCardCode
      game
      (PlayerCard searchingForIzzie)

  it "takes 2 actions and is discarded on a successful investigation" $ do
    investigator <- testInvestigator "00000" id
    searchingForIzzie <- buildPlayerCard "02011"
    location <- testLocation "00000" id
    game <- runGameTest
      investigator
      [ SetTokens [Zero]
      , loadDeck investigator [searchingForIzzie]
      , drawCards investigator 1
      , moveTo investigator location
      ]
      (locationsL %~ insertEntity location)
    let updatedSearchingForIzzie = game ^?! treacheriesL . to toList . ix 0

    [searchingForIzzieAction] <- getActionsOf
      game
      investigator
      NonFast
      updatedSearchingForIzzie

    game' <-
      runGameTestMessages game [searchingForIzzieAction]
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"
    updated game' location
      `shouldSatisfy` not
      . hasTreacheryWithMatchingCardCode game' (PlayerCard searchingForIzzie)
    updatedSearchingForIzzie `shouldSatisfy` isInDiscardOf game' investigator
    actionsRemaining (updated game' investigator) `shouldBe` 1
    updated game investigator `shouldSatisfy` hasTrauma (0, 0)

  it "causes 1 mental trauma if not discarded" $ do
    investigator <- testInvestigator "00000" id
    searchingForIzzie <- buildPlayerCard "02011"
    location <- testLocation "00000" id
    game <- runGameTest
      investigator
      [ loadDeck investigator [searchingForIzzie]
      , drawCards investigator 1
      , EndOfGame
      ]
      (locationsL %~ insertEntity location)
    updated game investigator `shouldSatisfy` hasTrauma (0, 1)
