module Arkham.Asset.Assets.MiguelsKnapsackSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Miguel's Knapsack" do
  it "requires its reaction when a fight event only has a target at a connecting location" . gameTest $ \self -> do
    knapsack <- self `putAssetIntoPlay` Assets.miguelsKnapsack
    (here, connecting) <- testConnectedLocations id id
    self `moveTo` here
    enemy <- testEnemy
    enemy `spawnAt` connecting
    backstab <- genCard Events.backstab
    withProp @"resources" 3 self
    self `addToHand` backstab

    duringTurn self do
      asDefs self.playableCards `shouldReturn` [Events.backstab]
      self `playCard` backstab
      chooseOnlyOption "Miguel's Knapsack is required to supply a legal target"
      knapsack.exhausted `shouldReturn` True

  it "allows its reaction to be skipped when the event has a local target" . gameTest $ \self -> do
    knapsack <- self `putAssetIntoPlay` Assets.miguelsKnapsack
    (here, _connecting) <- testConnectedLocations id id
    self `moveTo` here
    enemy <- testEnemy
    enemy `spawnAt` here
    backstab <- genCard Events.backstab
    withProp @"resources" 3 self
    self `addToHand` backstab

    duringTurn self do
      self `playCard` backstab
      skip
      knapsack.exhausted `shouldReturn` False
