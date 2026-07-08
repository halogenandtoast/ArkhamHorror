module Arkham.UltimatumsAndBoons.BoonOfThothSpec (spec) where

import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of Thoth" $ do
  it "each investigator draws 1 additional card in their opening hand" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfThoth]
    cards <- testPlayerCards 10
    withProp @"deck" (Deck cards) self
    run $ DrawStartingHand (toId self)
    self.hand `shouldSatisfyM` ((== 6) . length)

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
    withUltimatumsAndBoonsDisabled [BoonOfThoth]
    cards <- testPlayerCards 10
    withProp @"deck" (Deck cards) self
    run $ DrawStartingHand (toId self)
    self.hand `shouldSatisfyM` ((== 5) . length)
