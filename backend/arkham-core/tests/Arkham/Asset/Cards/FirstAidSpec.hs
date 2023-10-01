{-# OPTIONS_GHC -Wno-type-defaults #-}

module Arkham.Asset.Cards.FirstAidSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (daisyWalker, rolandBanks)
import Arkham.Investigator.Types (tokensL)
import Arkham.Matcher (assetIs)
import Arkham.Token
import TestImport.New

default (Int)

spec :: Spec
spec = describe "First Aid" $ do
  it
    "uses a supply and heals 1 damage or horror from an investigator at your location"
    . gameTest
    $ \self -> do
      withProp @"damage" 1 self
      investigator2 <- addInvestigator rolandBanks (tokensL %~ setTokens Horror 1)
      investigator3 <- addInvestigator daisyWalker (tokensL %~ setTokens Horror 1)
      self `putCardIntoPlay` Assets.firstAid
      firstAid <- selectJust $ assetIs Assets.firstAid
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      investigator2 `moveTo` location1
      investigator3 `moveTo` location2
      [useFirstAid] <- firstAid.abilities
      self `useAbility` useFirstAid
      chooseTarget self
      self.damage `shouldReturn` 0
      investigator2.horror `shouldReturn` 1
      self `useAbility` useFirstAid
      chooseTarget investigator2
      investigator2.horror `shouldReturn` 0
      investigator3.horror `shouldReturn` 1
