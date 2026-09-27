module Arkham.Asset.Assets.ValeLanternBeaconOfHopeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheTwistedHollow qualified as Locations
import Arkham.Location.Types (revealedL)
import TestImport.New

spec :: Spec
spec = describe "Vale Lantern (Beacon of Hope)" do
  context
    "When an investigator at your location moves into and reveals a Forest location, exhaust Vale Lantern: They ignore that location's forced effect(s)."
    do
      -- No Place Like Home is only here to put a SECOND forced ability in the same
      -- "after you reveal a location" window. That window resolves one ability per
      -- pass, and the suppression used to die on the first pass, so Mushroom Grove's
      -- Forced test came back on the next one (#5782). With a single ability in the
      -- window the bug is invisible.
      it "keeps suppressing the location's Forced ability across the whole reveal window" . gameTest $ \self -> do
        (start, grove) <-
          testConnectedLocationsWithDef
            (defaultTestLocation, id)
            (Locations.mushroomGrove, revealedL .~ False)
        self `moveTo` start
        lantern <- self `putAssetIntoPlay` Assets.valeLanternBeaconOfHope
        _ <- self `putAssetIntoPlay` Assets.noPlaceLikeHome
        self `moveTo` grove
        useReactionOf lantern
        chooseOnlyOption "resolve No Place Like Home's forced ability"
        assertNoAbilityOf grove
