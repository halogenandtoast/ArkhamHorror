module Arkham.Asset.Assets.TwilightDiademCrownOfDyingLightSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Token
import TestImport.New

spec :: Spec
spec = describe "Twilight Diadem: Crown of Dying Light" $ do
  it "treats a bless you reveal as an elder sign" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"shroud" 4
    self `moveTo` location
    diadem <- self `putAssetIntoPlay` Assets.twilightDiademCrownOfDyingLight
    run $ PlaceTokens (TestSource mempty) (toTarget diadem) Offering 2
    setChaosTokens [BlessToken]
    self `investigate` location
    startSkillTest
    useReactionOf diadem
    applyResults
    -- the bless resolved as an elder sign (+0 here), not as +2 bless;
    -- 1 intellect vs shroud 4 still fails, but the offering was spent
    diadem.countTokens Offering `shouldReturn` 1

  it "cannot be used on another investigator's skill test" . gameTest $ \self -> do
    location <- testLocation & prop @"shroud" 4
    other <- addInvestigator rolandBanks & prop @"intellect" 1
    self `moveTo` location
    other `moveTo` location
    mine <- self `putAssetIntoPlay` Assets.twilightDiademCrownOfDyingLight
    theirs <- other `putAssetIntoPlay` Assets.twilightDiademCrownOfDyingLight
    run $ PlaceTokens (TestSource mempty) (toTarget mine) Offering 2
    run $ PlaceTokens (TestSource mempty) (toTarget theirs) Offering 2
    setChaosTokens [BlessToken]
    other `investigate` location
    startSkillTest
    -- Both seats are asked at once, so `assertNoReactionOf` must scan every
    -- pending question -- plain `assertNoReaction` bails out whenever there is
    -- more than one and would pass vacuously here. `theirs` is the control:
    -- the performer's own diadem must still be usable, proving the reveal
    -- window is live and that the assertion above has something to look at.
    assertNoReactionOf mine
    useReactionOf theirs
    applyResults
    mine.countTokens Offering `shouldReturn` 2
    theirs.countTokens Offering `shouldReturn` 1
