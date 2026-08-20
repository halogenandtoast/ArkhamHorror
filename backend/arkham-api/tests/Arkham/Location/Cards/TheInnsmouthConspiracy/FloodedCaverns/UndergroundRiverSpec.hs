module Arkham.Location.Cards.TheInnsmouthConspiracy.FloodedCaverns.UndergroundRiverSpec (spec) where

import Arkham.Location.CardDefs.TheInnsmouthConspiracy.FloodedCaverns qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Location.Types (Field (LocationFloodLevel), revealedL)
import Arkham.Projection (field)
import TestImport.New

{- | 'testLocationWithDef' writes the location straight into the game entities, and
the harness only preloads modifiers once, before the body runs. 'runMessages'
preloads *after* running each message, so the very first message would see a
'gameModifiers' map that predates the river and read no 'CannotBeFullyFlooded'.
The 'tick' refreshes it.
-}
undergroundRiver :: TestAppT Location
undergroundRiver = testLocationWithDef Locations.undergroundRiver (revealedL .~ True) <* tick

spec :: Spec
spec = describe "Underground River" do
  it "cannot be fully flooded by an effect that sets its flood level directly" . gameTest $ \_self -> do
    -- The Water Rises (07043b) sets each revealed location to fully flooded, but
    -- Underground River's "cannot be fully flooded" caps it at partially flooded.
    river <- undergroundRiver
    run $ SetFloodLevel (toId river) FullyFlooded
    field LocationFloodLevel (toId river) `shouldReturn` Just PartiallyFlooded

  it "still becomes partially flooded when set to partially flooded" . gameTest $ \_self -> do
    river <- undergroundRiver
    run $ SetFloodLevel (toId river) PartiallyFlooded
    field LocationFloodLevel (toId river) `shouldReturn` Just PartiallyFlooded
