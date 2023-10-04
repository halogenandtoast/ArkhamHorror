module Arkham.Event.Cards.WardOfProtectionSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
import Arkham.Scenario.Types
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Ward of Protection" $ do
  context "When you draw a non-weakness treachery card" $ do
    it "Cancels that card's revelation effect. Then take 1 horror" . gameTest $ \self -> do
      wardOfProtection <- genCard Events.wardOfProtection
      frozenInFear <- genEncounterCard Treacheries.frozenInFear
      -- NOTE: We generate another card here to prevent the deck from shuffling
      ancientEvils <- genEncounterCard Treacheries.ancientEvils
      withProp @"resources" 1 self
      withProp @"hand" [wardOfProtection] self
      run $ SetEncounterDeck (Deck [frozenInFear, ancientEvils])
      run $ InvestigatorDrawEncounterCard $ toId self
      chooseTarget wardOfProtection
      applyAllHorror
      self.horror `shouldReturn` 1
      assert $ selectNone AnyTreachery
      scenarioField ScenarioDiscard `shouldReturn` [frozenInFear]
