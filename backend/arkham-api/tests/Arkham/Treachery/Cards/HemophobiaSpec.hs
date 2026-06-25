module Arkham.Treachery.Cards.HemophobiaSpec (spec) where

import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (..))
import TestImport.New

-- Hemophobia (treachery) has a FORCED ability:
--   "After you take 1 or more damage at your location: Take 1 horror."
--
-- Regression coverage for #4927: a card that ENTERS PLAY during an open window
-- must NOT respond to that window's already-occurred triggering condition. If
-- Hemophobia is drawn (entering the threat area) *during* the after-damage
-- window of a damage event, it must NOT trigger its forced horror for THAT
-- damage. But on a later damage event (Hemophobia already in play) it MUST
-- trigger.
--
-- The fix (#4927) implements this with a monotonic per-window "tick" clock: a
-- card records the tick at which it entered play (gameEntryTicks), and a
-- forced/reaction ability only matches a window whose open-tick is strictly
-- greater than its source card's entry-tick (see Arkham.Helpers.Action
-- getActionsWith and Arkham.Game.Runner entry-tick capture).
--
-- HARNESS LIMITATION: a literal "card drawn during an open window" ordering
-- cannot be expressed with a single card in the unit harness -- the only way to
-- inject an entry mid-window is via a reaction/forced ability firing inside that
-- window (which the window itself would have to offer). The window-tick clock is
-- also monotonic, so any window opened *after* a black-box reveal always has a
-- strictly greater tick and would therefore never be suppressed. The negative
-- case below therefore reproduces the exact tick relationship the engine
-- produces for a mid-window entry by pinning Hemophobia's entry-tick to the
-- currently-open window's tick, then driving the real suppression branch in
-- getActionsWith. End-to-end "drawn during the after-damage window" coverage
-- should come from arkham-replay against a recorded game.

spec :: Spec
spec = describe "Hemophobia" $ do
  context "After you take 1 or more damage at your location (forced)" $ do
    it "deals 1 horror when already in play before the damage (positive control)"
      . gameTest
      $ \self -> do
        location <- testLocation
        self `moveTo` location
        -- Hemophobia enters the threat area BEFORE any damage, so the
        -- after-damage window opens at a strictly later tick than its entry.
        self `drawsCard` Treacheries.hemophobia
        run $ assignDamage (toId self) (TestSource mempty) 1
        applyAllDamage
        useForcedAbility
        applyAllHorror
        self.horror `shouldReturn` 1
        self.damage `shouldReturn` 1

    it "does not deal horror for the damage event it entered play during (#4927)"
      . gameTest
      $ \self -> do
        location <- testLocation
        self `moveTo` location
        self `drawsCard` Treacheries.hemophobia
        hemophobia <- selectJust $ treacheryIs Treacheries.hemophobia
        hemophobiaCard <- field TreacheryCard hemophobia
        -- Simulate "entered play during the open after-damage window": pin the
        -- entry-tick to a value no opened window's tick can exceed, so the
        -- suppression branch in getActionsWith (openTick > entryTick) is False.
        overTest $ entryTicksL %~ insertMap (toCardId hemophobiaCard) maxBound
        run $ assignDamage (toId self) (TestSource mempty) 1
        applyAllDamage
        -- The forced ability must NOT be offered for this damage.
        assertHasNoReaction
        applyAllHorror
        self.horror `shouldReturn` 0
        self.damage `shouldReturn` 1
