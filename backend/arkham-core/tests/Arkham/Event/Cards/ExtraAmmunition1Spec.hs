module Arkham.Event.Cards.ExtraAmmunition1Spec (
  spec,
) where

import TestImport

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (assetIs)

spec :: Spec
spec = describe "Extra Ammunition (1)" $ do
  it
    "places 3 ammunition on a firearm asset controlled by an investigator at your location"
    $ gameTest
    $ \investigator -> do
      investigator2 <- addInvestigator Investigators.rolandBanks id
      location <- testLocation id
      pushAndRunAll
        [ moveTo investigator location
        , moveTo investigator2 location
        ]
      putCardIntoPlay investigator Assets.fortyFiveAutomatic
      putCardIntoPlay investigator Events.extraAmmunition1
      fortyFiveAutomatic <- selectJust $ assetIs Assets.fortyFiveAutomatic
      fieldAssert AssetUses (== Uses Ammo 7) fortyFiveAutomatic
