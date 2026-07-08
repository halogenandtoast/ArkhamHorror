module Arkham.UltimatumsAndBoons.BoonOfHadesSpec (spec) where

import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of Hades" $ do
  it "each investigator begins the game with 2 additional resources" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfHades]
    run $ TakeStartingResources (toId self)
    self.resources `shouldReturn` 7

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
    withUltimatumsAndBoonsDisabled [BoonOfHades]
    run $ TakeStartingResources (toId self)
    self.resources `shouldReturn` 5
