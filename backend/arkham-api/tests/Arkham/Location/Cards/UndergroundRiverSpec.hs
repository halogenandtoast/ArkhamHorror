module Arkham.Location.Cards.UndergroundRiverSpec (spec) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Location.Types (Field (LocationFloodLevel), revealedL)
import Arkham.Projection (field)
import TestImport.New

spec :: Spec
spec = describe "Underground River" do
  it "cannot be fully flooded by an effect that sets its flood level directly" . gameTest $ \_self -> do
    -- The Water Rises (07043b) sets each revealed location to fully flooded, but
    -- Underground River's "cannot be fully flooded" caps it at partially flooded.
    river <- testLocationWithDef Locations.undergroundRiver (revealedL .~ True)
    run $ SetFloodLevel (toId river) FullyFlooded
    field LocationFloodLevel (toId river) `shouldReturn` Just PartiallyFlooded

  it "still becomes partially flooded when set to partially flooded" . gameTest $ \_self -> do
    river <- testLocationWithDef Locations.undergroundRiver (revealedL .~ True)
    run $ SetFloodLevel (toId river) PartiallyFlooded
    field LocationFloodLevel (toId river) `shouldReturn` Just PartiallyFlooded
