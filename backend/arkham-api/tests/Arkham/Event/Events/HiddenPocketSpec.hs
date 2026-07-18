module Arkham.Event.Events.HiddenPocketSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Hidden Pocket" do
  it "does not require Fence when it is already fast and affordable" . gameTest $ \self -> do
    _leatherCoat <- self `putAssetIntoPlay` Assets.leatherCoat
    _fence <- self `putAssetIntoPlay` Assets.fence1
    hiddenPocket <- genCard Events.hiddenPocket
    withProp @"resources" 1 self
    self `addToHand` hiddenPocket

    duringTurn self do
      self `playCard` hiddenPocket
      skip
      chooseOnlyOption "attach Hidden Pocket to Leather Coat"
      clickLabel "$label.addHandSlot"
