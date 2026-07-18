module Arkham.Treachery.Cards.BetweenWorldsSpec (spec) where

import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Types
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Between Worlds" do
  it "does not enter the discard while it is in play as a location" . gameTest $ \self -> do
    (nexus, placement) <- placeLocationCard Locations.nexusOfNKai
    betweenWorlds <- genEncounterCard Treacheries.betweenWorlds
    ancientEvils <- genEncounterCard Treacheries.ancientEvils

    run placement
    self `moveTo` nexus
    run $ SetEncounterDeck (Deck [betweenWorlds, ancientEvils])
    run $ drawEncounterCard self.id GameSource

    chooseOnlyOption "resolve Nexus of N'Kai"
    applyAllHorror

    assertAny $ locationIs Locations.betweenWorlds
    scenarioField ScenarioDiscard `shouldReturn` []
