module Arkham.Treachery.Cards.PrismaticPhenomenonSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Types (revealedL)
import Arkham.Matcher (TreacheryMatcher (TreacheryWithId))
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Prismatic Phenomenon" $ do
  it "replaces both Sixth Sense discoveries and discards itself" . gameTest $ \self -> do
    withProp @"willpower" 5 self
    (currentLocation, connectingLocation) <-
      testConnectedLocations (revealedL .~ True) (revealedL .~ True)
    updateProp @"shroud" 0 currentLocation
    updateProp @"clues" 1 currentLocation
    updateProp @"shroud" 0 connectingLocation
    updateProp @"clues" 1 connectingLocation
    self `moveTo` currentLocation

    sixthSense <- self `putAssetIntoPlay` Assets.sixthSense4
    prismaticPhenomenon <- self `putTreacheryIntoPlay` Treacheries.prismaticPhenomenon
    setChaosTokens [Skull]

    [investigateAction] <- self `getActionsFrom` sixthSense
    self `useAbility` investigateAction
    startSkillTest
    chooseTarget connectingLocation
    clickLabel "$label.useOriginalLocationsShroud"
    applyResults
    useForcedAbility
    chooseFirstOption "resolve the first location's investigation result"

    assertNone $ TreacheryWithId prismaticPhenomenon
    currentLocation.clues `shouldReturn` 1
    connectingLocation.clues `shouldReturn` 1
    self.clues `shouldReturn` 0
