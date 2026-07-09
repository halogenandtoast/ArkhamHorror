module Arkham.UltimatumsAndBoons.UltimatumOfHardshipSpec (spec) where

import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of Hardship" $ do
  it "each investigator begins the game with 2 fewer resources" . gameTest $ \self -> do
    withUltimatums [UltimatumOfHardship]
    run $ TakeStartingResources (toId self)
    self.resources `shouldReturn` 3

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
    withUltimatumsDisabled [UltimatumOfHardship]
    run $ TakeStartingResources (toId self)
    self.resources `shouldReturn` 5
