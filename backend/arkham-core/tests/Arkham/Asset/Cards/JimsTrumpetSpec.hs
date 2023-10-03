module Arkham.Asset.Cards.JimsTrumpetSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Jim's Trumpet" $ do
  context "allows you to heal one horror when skull is revealed" $ do
    it "on yourself" $ gameTest $ \self -> do
      withProp @"horror" 1 self
      self `putCardIntoPlay` Assets.jimsTrumpet
      location <- testLocation
      setChaosTokens [Skull]
      self `moveTo` location
      runSkillTest self #intellect 0
      useReaction
      click "choose self"
      self.horror `shouldReturn` 0

    it "on an investigator at your location" $ gameTest $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks & prop @"horror" 1
      self `putCardIntoPlay` Assets.jimsTrumpet
      location <- testLocation
      setChaosTokens [Skull]
      moveAllTo location
      runSkillTest self #intellect 0
      useReaction
      click "choose investigator at same location"
      investigator2.horror `shouldReturn` 0

    it "even when another player draws token" $ gameTest $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks & prop @"horror" 1
      self `putCardIntoPlay` Assets.jimsTrumpet
      location <- testLocation
      setChaosTokens [Skull]
      moveAllTo location
      runSkillTest investigator2 SkillIntellect 0
      useReaction
      click "choose investigator at same location"
      investigator2.horror `shouldReturn` 0

    it "on an investigator at a connected location" $ gameTest $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks & prop @"horror" 1
      self `putCardIntoPlay` Assets.jimsTrumpet
      rivertown <- testLocationWithDef Locations.rivertown id
      southside <- testLocationWithDef Locations.southsideHistoricalSociety id
      setChaosTokens [Skull]
      run $ placedLocation rivertown
      run $ placedLocation southside
      self `moveTo` rivertown
      investigator2 `moveTo` southside
      runSkillTest self #intellect 0
      useReaction
      click "choose investigator at connected location"
      investigator2.horror `shouldReturn` 0

    it "cannot target an investigator at an unconnected location" $ gameTest $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks & prop @"horror" 1
      self `putCardIntoPlay` Assets.jimsTrumpet
      rivertown <- testLocationWithDef Locations.rivertown id
      downtown <- testLocationWithDef Locations.downtownArkhamAsylum id
      setChaosTokens [Skull]
      run $ placedLocation rivertown
      run $ placedLocation downtown
      self `moveTo` rivertown
      investigator2 `moveTo` downtown
      runSkillTest self #intellect 0
      chooseOnlyOption "apply results"
      investigator2.horror `shouldReturn` 1
