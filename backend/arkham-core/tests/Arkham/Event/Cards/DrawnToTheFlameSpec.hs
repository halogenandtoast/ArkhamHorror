module Arkham.Event.Cards.DrawnToTheFlameSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Location.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Drawn to the flame" $ do
  it "draws the top card of the encounter deck and then you discover two clues" . gameTest $ \self -> do
    -- We use "On Wings of Darkness" here to check that the Revelation effect
    -- resolves and that the clues discovered are at your location after the
    -- effect per the FAQ
    withProp @"agility" 3 self
    rivertown <- testLocationWithDef Cards.rivertown id
    southside <- testLocationWithDef Cards.southsideHistoricalSociety id
    onWingsOfDarkness <- genEncounterCard Cards.onWingsOfDarkness
    run $ SetEncounterDeck (Deck [onWingsOfDarkness])
    setChaosTokens [Zero]
    run $ PlaceClues GameSource (toTarget rivertown) 1
    self `moveTo` southside
    self `playEvent` Events.drawnToTheFlame
    startSkillTest
    applyResults
    chooseFirstOption "apply horror/damage"
    chooseFirstOption "apply horror/damage"
    chooseOnlyOption "move to central location"
    self.clues `shouldReturn` 2
    assert $ Events.drawnToTheFlame `isInDiscardOf` self
