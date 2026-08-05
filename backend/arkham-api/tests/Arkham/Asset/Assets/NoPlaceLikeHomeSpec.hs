module Arkham.Asset.Assets.NoPlaceLikeHomeSpec (spec) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Location.Types (revealedL)
import Arkham.Modifier
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "No Place Like Home" do
  describe "Lost and Uncertain" do
    it "removes 1 discovery when you reveal a location" . gameTest $ \self -> do
      location <- testLocationWith (revealedL .~ False)
      nplh <- self `putAssetIntoPlay` Assets.noPlaceLikeHome
      run $ RevealLocation (Just $ toId self) (toId location)
      chooseOnlyOption "resolve No Place Like Home's forced ability"
      fieldMap AssetUses (findWithDefault 0 Discovery) nplh `shouldReturn` 7

    -- "1 [per_investigator] or fewer discoveries" — with one investigator the
    -- threshold is 1, so 1 left still marks progress and 2 left does not.
    it "marks 1 progress at 1 per investigator discovery" . gameTest $ \self -> do
      nplh <- self `putAssetIntoPlay` Assets.noPlaceLikeHome
      run $ SpendUses (TestSource mempty) (toTarget nplh) Discovery 7
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.NoPlaceLikeHome `shouldReturn` 1

    it "marks no progress above 1 per investigator discovery" . gameTest $ \self -> do
      nplh <- self `putAssetIntoPlay` Assets.noPlaceLikeHome
      run $ SpendUses (TestSource mempty) (toTarget nplh) Discovery 6
      endGame
      self `getActionsFrom` nplh `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.NoPlaceLikeHome `shouldReturn` 0

  describe "Where Your Heart Is" do
    it "grants an additional move-only action" . gameTest $ \self -> do
      nplh <- self `putAssetIntoPlay` Assets.noPlaceLikeHomeCompleted
      getModifiers self
        `shouldContainM` [ GiveAdditionalAction
                             $ AdditionalAction
                               "No Place Like Home"
                               (AssetSource nplh)
                               (ActionRestrictedAdditionalAction #move)
                         ]
