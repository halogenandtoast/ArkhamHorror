module Arkham.Asset.Assets.FieldAgent2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Field Agent (2)" do
  -- Issue #5262: the ability was gated on CanDiscoverCluesAt, which only checks
  -- the CannotDiscoverClues* modifiers, so it was offered on a location with no
  -- clues and the player could pay exhaust + 1 horror for nothing.
  context "when there are no clues at your location" do
    it "does not offer its ability" . gameTest $ \self -> do
      fieldAgent2 <- self `putAssetIntoPlay` Assets.fieldAgent2
      location <- testLocation & prop @"clues" 0
      duringTurn self do
        self `moveTo` location
        (self `getActionsFrom` fieldAgent2) `shouldReturn` []

  context "when there is a clue at your location" do
    it "exhausts and takes 1 horror to discover 1 clue" . gameTest $ \self -> do
      fieldAgent2 <- self `putAssetIntoPlay` Assets.fieldAgent2
      location <- testLocation & prop @"clues" 1
      duringTurn self do
        self `moveTo` location
        [discoverClue] <- self `getActionsFrom` fieldAgent2
        self `useAbility` discoverClue
        self.clues `shouldReturn` 1
        location.clues `shouldReturn` 0
        fieldAgent2.horror `shouldReturn` 1
        assert fieldAgent2.exhausted
