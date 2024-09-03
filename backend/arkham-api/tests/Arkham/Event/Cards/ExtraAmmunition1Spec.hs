module Arkham.Event.Cards.ExtraAmmunition1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Extra Ammunition (1)" $ do
  it "places 3 ammunition on a firearm asset controlled by an investigator at your location" . gameTest $ \self -> do
    investigator2 <- addInvestigator Investigators.rolandBanks
    location <- testLocationWith id
    self `moveTo` location
    investigator2 `moveTo` location
    fortyFiveAutomatic <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic
    fortyFiveAutomatic.uses `shouldReturn` singletonMap Ammo 4
    self `playEvent` Events.extraAmmunition1
    fortyFiveAutomatic.uses `shouldReturn` singletonMap Ammo 7
