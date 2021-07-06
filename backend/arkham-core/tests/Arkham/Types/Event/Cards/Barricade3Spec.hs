module Arkham.Types.Event.Cards.Barricade3Spec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Modifier

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it
        "should make the current location unenterable by non elites and non elites cannot spawn there"
      $ do
          location <- testLocation id
          investigator <- testInvestigator "00000" id
          barricade3 <- buildEvent "50004" investigator
          gameTest
              investigator
              [moveTo investigator location, playEvent investigator barricade3]
              ((eventsL %~ insertEntity barricade3)
              . (locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                (map modifierType
                  <$> getModifiersFor (TestSource mempty) (toTarget location) ()
                  )
                  `shouldReturn` [ CannotBeEnteredByNonElite
                                 , SpawnNonEliteAtConnectingInstead
                                 ]
                isAttachedTo location barricade3 `shouldReturn` True

    it "should be discarded if an investigator leaves the location" $ do
      location <- testLocation id
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      barricade3 <- buildEvent "01038" investigator
      gameTest
          investigator
          [ moveAllTo location
          , playEvent investigator barricade3
          , moveFrom investigator2 location
          ]
          ((eventsL %~ insertEntity barricade3)
          . (locationsL %~ insertEntity location)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            getModifiersFor (TestSource mempty) (toTarget location) ()
              `shouldReturn` []
            isAttachedTo location barricade3 `shouldReturn` False
            isInDiscardOf investigator barricade3 `shouldReturn` True
