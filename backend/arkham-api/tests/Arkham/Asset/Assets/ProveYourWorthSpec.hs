module Arkham.Asset.Assets.ProveYourWorthSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Prove Your Worth" do
  describe "Are You a Burden?" do
    it "removes 1 chance when you fail a skill test by 2 or more" . gameTest $ \self -> do
      proveYourWorth <- self `putAssetIntoPlay` Assets.proveYourWorth
      setChaosTokens [Zero]
      sid <- getRandom
      runSkillTest sid self #combat 10
      applyResults
      useForcedAbility
      fieldMap AssetUses (findWithDefault 0 Chance) proveYourWorth `shouldReturn` 3

    it "marks 1 progress when the game ends with chances remaining" . gameTest $ \self -> do
      proveYourWorth <- self `putAssetIntoPlay` Assets.proveYourWorth
      run $ SpendUses (TestSource mempty) (toTarget proveYourWorth) Chance 3
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.ProveYourWorth `shouldReturn` 1

    it "marks no progress when every chance has been spent" . gameTest $ \self -> do
      proveYourWorth <- self `putAssetIntoPlay` Assets.proveYourWorth
      run $ SpendUses (TestSource mempty) (toTarget proveYourWorth) Chance 4
      endGame
      self `getActionsFrom` proveYourWorth `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.ProveYourWorth `shouldReturn` 0

  describe "Standing on Your Own" do
    it "spends 1 chance to cancel a revealed token" . gameTest $ \self -> do
      proveYourWorth <- self `putAssetIntoPlay` Assets.proveYourWorthCompleted
      setChaosTokens [MinusEight]
      sid <- getRandom
      runSkillTest sid self #willpower 1
      useReaction
      fieldMap AssetUses (findWithDefault 0 Chance) proveYourWorth `shouldReturn` 2
