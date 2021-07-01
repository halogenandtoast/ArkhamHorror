module Arkham.Types.Event.Cards.DrawnToTheFlameSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Types.Location.Attrs (cluesL)
import Arkham.Types.Trait

spec :: Spec
spec = describe "Drawn to the flame" $ do
  it "draws the top card of the encounter deck and then you discover two clues"
    $ do
    -- We use "On Wings of Darkness" here to check that the Revelation effect
    -- resolves and that the clues discovered are at your location after the
    -- effect per the FAQ
        investigator <- testInvestigator "00000"
          $ \attrs -> attrs { investigatorAgility = 3 }
        (startLocation, centralLocation) <-
          testConnectedLocationsWithDef (id, id) (cardTraitsL .~ singleton Central, cluesL .~ 2)
        drawnToTheFlame <- buildEvent "01064" investigator
        onWingsOfDarkness <- buildEncounterCard "01173"
        gameTest
            investigator
            [ SetEncounterDeck [onWingsOfDarkness]
            , SetTokens [Zero]
            , placedLocation startLocation
            , placedLocation centralLocation
            , moveTo investigator startLocation
            , playEvent investigator drawnToTheFlame
            ]
            ((eventsL %~ insertEntity drawnToTheFlame)
            . (locationsL %~ insertEntity startLocation)
            . (locationsL %~ insertEntity centralLocation)
            )
          $ do
              runMessages
              chooseOnlyOption "start skill test"
              chooseOnlyOption "apply results"
              chooseFirstOption "apply horror/damage"
              chooseFirstOption "apply horror/damage"
              chooseOnlyOption "move to central location"
              updated investigator `shouldSatisfyM` hasClueCount 2
              isInDiscardOf investigator drawnToTheFlame `shouldReturn` True
