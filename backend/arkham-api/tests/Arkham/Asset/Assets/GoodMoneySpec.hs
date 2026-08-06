module Arkham.Asset.Assets.GoodMoneySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetResources))
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window qualified as Window
import TestImport.New

spec :: Spec
spec = describe "Good Money" do
  describe "The \"Quid\" Part" do
    it "places 1 of the gained resources on Good Money" . gameTest $ \self -> do
      goodMoney <- self `putAssetIntoPlay` Assets.goodMoney
      self `gainResources` 3
      useReaction
      field AssetResources goodMoney `shouldReturn` 1
      self.resources `shouldReturn` 2

    -- The upkeep resource used to bypass TakeResources entirely, so it opened no
    -- GainsResources window and this reaction never fired during upkeep.
    it "also triggers on the resource gained during upkeep" . gameTest $ \self -> do
      goodMoney <- self `putAssetIntoPlay` Assets.goodMoney
      -- The default test investigator is Jenny Barnes, whose UpkeepResources
      -- modifier makes the upkeep gain 2 rather than 1.
      run AllDrawCardAndResource
      self.resources `shouldReturn` 2
      useReaction
      field AssetResources goodMoney `shouldReturn` 1
      self.resources `shouldReturn` 1

    it "marks 1 progress when the game ends with 5 or more resources on it" . gameTest $ \self -> do
      goodMoney <- self `putAssetIntoPlay` Assets.goodMoney
      run $ PlaceTokens GameSource (toTarget goodMoney) #resource 5
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.GoodMoney `shouldReturn` 1

    -- A Task leaves play with its investigator, so by the time the resolution
    -- opens the end-of-game window a resigned investigator's Task is already
    -- gone. It has to mark progress off the elimination window instead.
    it "marks 1 progress when you resign with 5 or more resources on it" . gameTest $ \self -> do
      goodMoney <- self `putAssetIntoPlay` Assets.goodMoney
      run $ PlaceTokens GameSource (toTarget goodMoney) #resource 5
      run $ CheckWindows [Window.mkWhen (Window.InvestigatorEliminated $ toId self)]
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.GoodMoney `shouldReturn` 1

    it "marks no progress when the game ends with fewer than 5 resources on it" . gameTest $ \self -> do
      goodMoney <- self `putAssetIntoPlay` Assets.goodMoney
      run $ PlaceTokens GameSource (toTarget goodMoney) #resource 4
      endGame
      self `getActionsFrom` goodMoney `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.GoodMoney `shouldReturn` 0

  describe "The \"Quo\" Part" do
    it "grants 5 additional starting resources" . gameTest $ \self -> do
      _ <- self `putAssetIntoPlay` Assets.goodMoneyCompleted
      getModifiers self `shouldContainM` [StartingResources 5]
