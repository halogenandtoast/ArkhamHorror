module Arkham.Location.TypesSpec (spec) where

import Arkham.Location.CardDefs.TheDrownedCity.TheWesternWall qualified as Locations
import Arkham.Location.Types (Field (LocationJustShroud, LocationPrintedShroud))
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "LocationPrintedShroud" do
  it "resolves a printed X while the location is in play" . gameTest $ \_ -> do
    (westernWall, placement) <- placeLocationCard Locations.westernWall_11530
    run placement

    field LocationPrintedShroud westernWall `shouldReturn` Just (Static 1)

  it "does not include shroud modifiers for a printed non-X value" . gameTest $ \_ -> do
    location <- testLocation & prop @"shroud" 4
    run =<< gameModifier (TestSource mempty) (toTarget location) (ShroudModifier 2)

    field LocationPrintedShroud (toId location) `shouldReturn` Just (Static 4)

  it "preserves shroud increases when reductions are prohibited" . gameTest $ \_ -> do
    location <- testLocation & prop @"shroud" 3
    for_ [ShroudCannotBeReduced, ShroudModifier 2, ShroudModifier (-2)]
      $ run
      <=< gameModifier (TestSource mempty) (toTarget location)

    field LocationJustShroud (toId location) `shouldReturn` 5
