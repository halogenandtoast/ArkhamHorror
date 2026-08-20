module Arkham.Treachery.Cards.UnawareSpec (spec) where

import Arkham.Enemy.CardDefs.NightOfTheZealot.Rats qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.CardDefs.Standalone qualified as Treacheries
import TestImport.New

-- Unaware (60356) has a FORCED ability:
--   "The first time you fail a skill test during your turn: Draw the top card of
--    the encounter deck. If that card is an enemy, discard Unaware."
--
-- Regression coverage for #5429: the "during your turn" clause was missing, so
-- the ability also fired for failures outside the investigator's turn (the
-- report was a Mythos-phase revelation test).

spec :: Spec
spec = describe "Unaware" do
  context "forced ability" do
    it "draws the top card of the encounter deck when you fail a test during your turn" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      rats <- genEncounterCard Cards.swarmOfRats
      run $ SetEncounterDeck (Deck [rats])
      self `drawsCard` Treacheries.unaware
      failSkillTest self
      useForcedAbility
      assertAny $ enemyIs Cards.swarmOfRats
      asDefs self.discard `shouldReturn` [Treacheries.unaware]

    it "does not trigger when the failed test happens outside your turn (#5429)" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      rats <- genEncounterCard Cards.swarmOfRats
      run $ SetEncounterDeck (Deck [rats])
      self `drawsCard` Treacheries.unaware
      -- Mirror the reported state: a failure resolved with no turn player (the
      -- engine clears this outside the investigation phase).
      overTest $ turnPlayerInvestigatorIdL .~ Nothing
      failSkillTest self
      assertHasNoReaction
      assertNone $ enemyIs Cards.swarmOfRats
      assertAny $ treacheryIs Treacheries.unaware
