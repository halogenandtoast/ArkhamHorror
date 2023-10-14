module Arkham.Asset.Cards.SegmentOfOnyx1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Segment of Onyx (1)" do
  context "If you have 3 copies of Segment of Onyx in play" do
    context "Fast ability" do
      it
        "Set them aside, out of play, search your bonded cards for Pendant of the Queen and put it into play"
        . gameTest
        $ \self -> do
          pendantOfTheQueen <- genCard Assets.pendantOfTheQueen
          withProp @"bonded" [pendantOfTheQueen] self
          self `putCardIntoPlay` Assets.segmentOfOnyx1
          self `putCardIntoPlay` Assets.segmentOfOnyx1
          segmentOfOnyx <- self `putAssetIntoPlay` Assets.segmentOfOnyx1
          [useSegments] <- self `getActionsFrom` segmentOfOnyx
          self `useAbility` useSegments
          assertNone $ assetIs Assets.segmentOfOnyx1
          assertAny $ assetIs Assets.pendantOfTheQueen <> AssetWithCardId (toCardId pendantOfTheQueen)
