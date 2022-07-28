module Arkham.Event.Cards.DrawnToTheFlameSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Location.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Investigator.Types (Field(..), InvestigatorAttrs(..))

spec :: Spec
spec = describe "Drawn to the flame" $ do
  it "draws the top card of the encounter deck and then you discover two clues"
    $ do
    -- We use "On Wings of Darkness" here to check that the Revelation effect
    -- resolves and that the clues discovered are at your location after the
    -- effect per the FAQ
        investigator <- testJenny
          $ \attrs -> attrs { investigatorAgility = 3 }
        rivertown <- createLocation <$> genEncounterCard Cards.rivertown
        southside <- createLocation
          <$> genEncounterCard Cards.southsideHistoricalSociety
        drawnToTheFlame <- buildEvent Events.drawnToTheFlame investigator
        onWingsOfDarkness <- genEncounterCard Cards.onWingsOfDarkness
        gameTest
            investigator
            [ SetEncounterDeck (Deck [onWingsOfDarkness])
            , SetTokens [Zero]
            , placedLocation rivertown
            , placedLocation southside
            , PlaceClues (toTarget rivertown) 1
            , moveTo investigator southside
            , playEvent investigator drawnToTheFlame
            ]
            ((entitiesL . eventsL %~ insertEntity drawnToTheFlame)
            . (entitiesL . locationsL %~ insertEntity rivertown)
            . (entitiesL . locationsL %~ insertEntity southside)
            )
          $ do
              runMessages
              chooseOnlyOption "start skill test"
              chooseOnlyOption "apply results"
              chooseFirstOption "apply horror/damage"
              chooseFirstOption "apply horror/damage"
              chooseOnlyOption "move to central location"
              fieldAssert InvestigatorClues (== 2) investigator
              isInDiscardOf investigator drawnToTheFlame `shouldReturn` True
