module Arkham.Event.Cards.DrawnToTheFlameSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field (..), InvestigatorAttrs (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Drawn to the flame" $ do
  it "draws the top card of the encounter deck and then you discover two clues" $ gameTest $ \investigator -> do
    -- We use "On Wings of Darkness" here to check that the Revelation effect
    -- resolves and that the clues discovered are at your location after the
    -- effect per the FAQ
    updateInvestigator investigator \attrs -> attrs {investigatorAgility = 3}
    rivertown <- testLocationWithDef Cards.rivertown id
    southside <- testLocationWithDef Cards.southsideHistoricalSociety id
    onWingsOfDarkness <- genEncounterCard Cards.onWingsOfDarkness
    pushAndRunAll
      [ SetEncounterDeck (Deck [onWingsOfDarkness])
      , SetTokens [Zero]
      , placedLocation rivertown
      , placedLocation southside
      , PlaceClues GameSource (toTarget rivertown) 1
      , moveTo investigator southside
      ]
    putCardIntoPlay investigator Events.drawnToTheFlame
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"
    chooseFirstOption "apply horror/damage"
    chooseFirstOption "apply horror/damage"
    chooseOnlyOption "move to central location"
    fieldAssert InvestigatorClues (== 2) investigator
    assert $ isInDiscardOf investigator Events.drawnToTheFlame
