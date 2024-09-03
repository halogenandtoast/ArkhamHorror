module Arkham.Asset.Cards.Encyclopedia2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

spec :: Spec
spec = describe "Encyclopedia (2)" $ do
  context "can be exhausted" $ do
    it "choose an investigator at your location, add +2 to a chosen skill until the end of the phase"
      . gameTest
      $ \self -> do
        withProp @"intellect" 0 self
        roland <- addInvestigator rolandBanks & prop @"intellect" 0
        location <- testLocation
        encyclopedia2 <- self `putAssetIntoPlay` Assets.encyclopedia2
        moveAllTo location

        withEach [self, roland] $ \target -> do
          let other = if target == self then roland else self
          duringPhase #investigation $ do
            [doExhaust] <- self `getActionsFrom` encyclopedia2
            self `useAbility` doExhaust
            chooseTarget target
            chooseSkill #intellect
            target.intellect `shouldReturn` 2
            other.intellect `shouldReturn` 0

          target.intellect `shouldReturn` 0
          other.intellect `shouldReturn` 0
