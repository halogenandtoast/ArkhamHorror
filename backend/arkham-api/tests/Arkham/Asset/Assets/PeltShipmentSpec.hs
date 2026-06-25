module Arkham.Asset.Assets.PeltShipmentSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

-- Pelt Shipment imposes "while in your hand, your hand size is reduced by 3".
-- That penalty must only apply while the card is *actually* in hand, not while
-- it is merely "as if in hand for play" under a host asset (e.g. Backpack). The
-- 45 Automatic is put into play afterwards purely to drive a preload pass so the
-- stashed card is loaded as an in-hand effect entity.
spec :: Spec
spec = describe "Pelt Shipment" do
  it "reduces max hand size by 3 while actually in hand" . gameTest $ \self -> do
    peltShipment <- genMyCard self Assets.peltShipment
    withProp @"hand" [peltShipment] self
    _ <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic
    assert $ self `hasModifier` HandSize (-3)

  it "does not reduce hand size while only stashed under Backpack" . gameTest $ \self -> do
    backpack <- self `putAssetIntoPlay` Assets.backpack
    peltShipment <- genMyCard self Assets.peltShipment
    run $ PlaceUnderneath (toTarget backpack) [peltShipment]
    _ <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic
    assert $ self `withoutModifier` HandSize (-3)
