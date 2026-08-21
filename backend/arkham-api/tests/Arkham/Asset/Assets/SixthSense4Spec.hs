module Arkham.Asset.Assets.SixthSense4Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.CardDefs.ThePathToCarcosa.EchoesOfThePast qualified as Locations
import Arkham.Location.Types (revealedL)
import TestImport.New

-- Investigating "in addition to" your location: AsIfAlsoAt, not AsIfAt.
spec :: Spec
spec = describe "Sixth Sense (4)" do
  it "lets you trigger the chosen location's abilities" . gameTest $ \self -> do
    withProp @"willpower" 5 self
    (currentLocation, library) <-
      testConnectedLocationsWithDef
        (defaultTestLocation, revealedL .~ True)
        (Locations.historicalSocietyHistoricalLibrary_136, revealedL .~ True)
    updateProp @"shroud" 0 currentLocation
    updateProp @"shroud" 0 library
    updateProp @"clues" 2 library
    self `moveTo` currentLocation

    sixthSense <- self `putAssetIntoPlay` Assets.sixthSense4
    setChaosTokens [Skull]

    [investigateAction] <- self `getActionsFrom` sixthSense
    self `useAbility` investigateAction
    startSkillTest
    chooseTarget library
    clickLabel "$label.useOriginalLocationsShroud"
    applyResults

    useReactionOf library
    chooseFirstOption "assign 2 horror"

    -- the cost was paid, so the remote location's Here-gated ability was reachable
    self.horror `shouldReturn` 2
    -- "in addition to": you never left your own location
    self.location `shouldReturn` Just (toId currentLocation)

  -- FAQ v2.5 Q63: one test, results applied to BOTH locations.
  it "applies the results of the test to both locations" . gameTest $ \self -> do
    withProp @"willpower" 5 self
    (currentLocation, connectingLocation) <-
      testConnectedLocations (revealedL .~ True) (revealedL .~ True)
    updateProp @"shroud" 0 currentLocation
    updateProp @"clues" 1 currentLocation
    updateProp @"shroud" 0 connectingLocation
    updateProp @"clues" 1 connectingLocation
    self `moveTo` currentLocation

    sixthSense <- self `putAssetIntoPlay` Assets.sixthSense4
    setChaosTokens [Skull]

    [investigateAction] <- self `getActionsFrom` sixthSense
    self `useAbility` investigateAction
    startSkillTest
    chooseTarget connectingLocation
    clickLabel "$label.useOriginalLocationsShroud"
    applyResults
    chooseFirstOption "resolve one location's investigation result"

    self.clues `shouldReturn` 2
    currentLocation.clues `shouldReturn` 0
    connectingLocation.clues `shouldReturn` 0
