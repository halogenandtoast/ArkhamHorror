module Arkham.Debug.ActionsSpec (spec) where

import TestImport.New

spec :: Spec
spec = describe "Debug action adjustments" do
  it "a negative gain takes actions away" . gameTest $ \self -> do
    withProp @"remainingActions" 3 self
    run $ GainActions (toId self) GameSource (-1)
    self.remainingActions `shouldReturn` 2

  it "never takes an investigator below zero actions" . gameTest $ \self -> do
    withProp @"remainingActions" 2 self
    run $ GainActions (toId self) GameSource (-5)
    self.remainingActions `shouldReturn` 0

  it "still gains actions as before" . gameTest $ \self -> do
    withProp @"remainingActions" 1 self
    run $ GainActions (toId self) GameSource 2
    self.remainingActions `shouldReturn` 3
