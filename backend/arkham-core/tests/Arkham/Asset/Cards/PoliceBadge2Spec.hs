module Arkham.Asset.Cards.PoliceBadge2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

spec :: Spec
spec = describe "Police Badge (2)" do
  gives @"willpower" Assets.policeBadge2 1
  context "fast ability" $ do
    context "during your turn" $ do
      it "can be discarded to give 2 additional actions this turn" . gameTest $ \self -> do
        policeBadge2 <- self `putAssetIntoPlay` Assets.policeBadge2
        location <- testLocation
        self `moveTo` location
        duringTurn self $ do
          [fastAbility] <- self `getActionsFrom` policeBadge2
          self `useAbility` fastAbility
          self.remainingActions `shouldReturn` 5

    context "another players turn at your location" $ do
      it "can be discarded to give 2 additional actions this turn" . gameTest $ \self -> do
        roland <- addInvestigator rolandBanks
        policeBadge2 <- self `putAssetIntoPlay` Assets.policeBadge2
        (location1, location2) <- testConnectedLocations id id
        self `moveTo` location1
        roland `moveTo` location2
        duringTurn roland $ do
          getActionsFrom self policeBadge2 `shouldReturn` []
          roland `moveTo` location1
          [fastAbility] <- self `getActionsFrom` policeBadge2
          self `useAbility` fastAbility
          roland.remainingActions `shouldReturn` 5
