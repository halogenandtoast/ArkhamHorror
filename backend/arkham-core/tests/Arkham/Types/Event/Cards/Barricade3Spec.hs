module Arkham.Types.Event.Cards.Barricade3Spec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it
        "should make the current location unenterable by non elites and non elites cannot spawn there"
      $ do
          location <- testLocation "00000" id
          investigator <- testInvestigator "00000" id
          barricade3 <- buildEvent "50004" investigator
          game <-
            runGameTest
              investigator
              [moveTo investigator location, playEvent investigator barricade3]
            $ (events %~ insertEntity barricade3)
            . (locations %~ insertEntity location)
          withGame
              game
              (map modifierType
              <$> getModifiersFor TestSource (toTarget location) ()
              )
            `shouldReturn` [ CannotBeEnteredByNonElite
                           , SpawnNonEliteAtConnectingInstead
                           ]
          barricade3 `shouldSatisfy` isAttachedTo game location

    it "should be discarded if an investigator leaves the location" $ do
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      barricade3 <- buildEvent "01038" investigator
      game <-
        runGameTest
          investigator
          [ moveAllTo location
          , playEvent investigator barricade3
          , moveFrom investigator2 location
          ]
        $ (events %~ insertEntity barricade3)
        . (locations %~ insertEntity location)
        . (investigators %~ insertEntity investigator2)
      withGame game (getModifiersFor TestSource (toTarget location) ())
        `shouldReturn` []
      barricade3 `shouldSatisfy` not . isAttachedTo game location
      barricade3 `shouldSatisfy` isInDiscardOf game investigator
