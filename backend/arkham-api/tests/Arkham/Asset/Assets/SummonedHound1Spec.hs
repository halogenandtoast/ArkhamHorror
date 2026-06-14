module Arkham.Asset.Assets.SummonedHound1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Summoned Hound (1)" $ do
  context "at a location with an additional cost to investigate" do
    -- Regression for #4786: the additional investigate cost must not be paid
    -- when the ability is activated. The player should be asked whether to
    -- fight or investigate first, and only pay the cost if they investigate.
    it "does not charge the additional investigate cost before the fight/investigate choice"
      . gameTest
      $ \self -> do
        withProp @"remainingActions" 3 self
        hound <- self `putAssetIntoPlay` Assets.summonedHound1
        enemy <- testEnemy
        -- Orne Library adds `AdditionalCostToInvestigate (ActionCost 1)` while revealed
        orneLibrary <- testLocationWithDef Locations.orneLibrary id
        setChaosTokens [Zero]
        enemy `spawnAt` orneLibrary
        self `moveTo` orneLibrary
        duringTurn self do
          [useHound] <- self `getActionsFrom` hound
          self `useAbility` useHound
          -- We are now stopped at the fight/investigate choice. With the bug the
          -- ActionCost 1 was already paid here (remainingActions would be 2).
          self.remainingActions `shouldReturn` 3

  context "choosing investigate at such a location" do
    it "pays the additional investigate cost after choosing investigate" . gameTest $ \self -> do
      withProp @"remainingActions" 3 self
      hound <- self `putAssetIntoPlay` Assets.summonedHound1
      enemy <- testEnemy
      orneLibrary <- testLocationWithDef Locations.orneLibrary id
      setChaosTokens [Zero]
      enemy `spawnAt` orneLibrary
      self `moveTo` orneLibrary
      duringTurn self do
        [useHound] <- self `getActionsFrom` hound
        self `useAbility` useHound
        clickLabel "$label.investigate"
        self.remainingActions `shouldReturn` 2
