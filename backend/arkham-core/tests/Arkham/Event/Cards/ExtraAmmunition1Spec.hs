module Arkham.Event.Cards.ExtraAmmunition1Spec
  ( spec
  ) where

import TestImport

import Arkham.Asset.Attrs (Field(..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = describe "Extra Ammunition (1)" $ do
  it
      "places 3 ammunition on a firearm asset controlled by an investigator at your location"
    $ do
        investigator <- testJenny id
        investigator2 <- testInvestigator Investigators.rolandBanks id
        location <- testLocation id
        fortyFiveAutomatic <- buildAsset Assets.fortyFiveAutomatic (Just investigator2)
        extraAmmunition1 <- buildEvent Events.extraAmmunition1 investigator
        gameTest
            investigator
            [ moveTo investigator location
            , moveTo investigator2 location
            , playAsset investigator2 fortyFiveAutomatic
            , playEvent investigator extraAmmunition1
            ]
            ((entitiesL . investigatorsL %~ insertEntity investigator2)
            . (entitiesL . locationsL %~ insertEntity location)
            . (entitiesL . assetsL %~ insertEntity fortyFiveAutomatic)
            . (entitiesL . eventsL %~ insertEntity extraAmmunition1)
            )
          $ do
              runMessages
              fieldAssert AssetUses (== Uses Ammo 7) fortyFiveAutomatic
