module Arkham.Types.Event.Cards.BarricadeSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Modifier

spec :: Spec
spec = do
  describe "Barricade" $ do
    it "should make the current location unenterable by non elites" $ do
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000" id
      barricade <- buildEvent "01038" investigator
      runGameTest
          investigator
          [moveTo investigator location, playEvent investigator barricade]
          ((eventsL %~ insertEntity barricade)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessagesNoLogging
            (map modifierType
              <$> getModifiersFor (TestSource mempty) (toTarget location) ()
              )
              `shouldReturn` [CannotBeEnteredByNonElite]
            isAttachedTo location barricade `shouldReturn` True

    it "should be discarded if an investigator leaves the location" $ do
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      barricade <- buildEvent "01038" investigator
      runGameTest
          investigator
          [ moveAllTo location
          , playEvent investigator barricade
          , moveFrom investigator2 location
          ]
          ((eventsL %~ insertEntity barricade)
          . (locationsL %~ insertEntity location)
          . (investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessagesNoLogging
            getModifiersFor (TestSource mempty) (toTarget location) ()
              `shouldReturn` []
            isAttachedTo location barricade `shouldReturn` False
            isInDiscardOf investigator barricade `shouldReturn` True
