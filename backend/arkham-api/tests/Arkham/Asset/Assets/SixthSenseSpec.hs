module Arkham.Asset.Assets.SixthSenseSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Game.Settings (AsIfRuling (Chapter2AsIfRuling), settingsAsIfRuling)
import Arkham.Location.CardDefs.ThePathToCarcosa.EchoesOfThePast qualified as Locations
import Arkham.Location.Types (revealedL)
import TestImport.New

-- Regression for #5480: the skill test moved to the chosen location but the
-- investigator did not, so its Here-gated abilities were unreachable.
spec :: Spec
spec = describe "Sixth Sense" do
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

    sixthSense <- self `putAssetIntoPlay` Assets.sixthSense
    setChaosTokens [Skull]

    [investigateAction] <- self `getActionsFrom` sixthSense
    self `useAbility` investigateAction
    startSkillTest
    chooseTarget library
    clickLabel "$label.useOriginalLocationsShroud"
    applyResults

    useReactionOf library
    chooseFirstOption "assign 2 horror"

    self.horror `shouldReturn` 2
    self.clues `shouldReturn` 2
    library.clues `shouldReturn` 0

  it "does not let you trigger your own location's abilities" . gameTest $ \self -> do
    withProp @"willpower" 5 self
    (library, connectingLocation) <-
      testConnectedLocationsWithDef
        (Locations.historicalSocietyHistoricalLibrary_136, revealedL .~ True)
        (defaultTestLocation, revealedL .~ True)
    updateProp @"shroud" 0 library
    updateProp @"clues" 2 library
    updateProp @"shroud" 0 connectingLocation
    self `moveTo` library

    sixthSense <- self `putAssetIntoPlay` Assets.sixthSense
    setChaosTokens [Skull]

    [investigateAction] <- self `getActionsFrom` sixthSense
    self `useAbility` investigateAction
    startSkillTest
    chooseTarget connectingLocation
    clickLabel "$label.useOriginalLocationsShroud"
    applyResults

    -- "instead of your location": you are no longer considered to be at the library
    assertHasNoReaction
    self.horror `shouldReturn` 0

  -- Grimoire (Chapter 2): the altered state covers only the ability being
  -- resolved, so the library's own reaction sees the actual location. Chapter 1
  -- (FAQ v2.5 Q63) is the case above, where it does trigger.
  it "does not reach the chosen location's abilities under the Chapter 2 ruling" . gameTest $ \self -> do
    overTest \g -> g {gameSettings = (gameSettings g) {settingsAsIfRuling = Chapter2AsIfRuling}}
    withProp @"willpower" 5 self
    (currentLocation, library) <-
      testConnectedLocationsWithDef
        (defaultTestLocation, revealedL .~ True)
        (Locations.historicalSocietyHistoricalLibrary_136, revealedL .~ True)
    updateProp @"shroud" 0 currentLocation
    updateProp @"shroud" 0 library
    updateProp @"clues" 2 library
    self `moveTo` currentLocation

    sixthSense <- self `putAssetIntoPlay` Assets.sixthSense
    setChaosTokens [Skull]

    [investigateAction] <- self `getActionsFrom` sixthSense
    self `useAbility` investigateAction
    startSkillTest
    chooseTarget library
    clickLabel "$label.useOriginalLocationsShroud"
    applyResults

    assertHasNoReaction
    self.horror `shouldReturn` 0
