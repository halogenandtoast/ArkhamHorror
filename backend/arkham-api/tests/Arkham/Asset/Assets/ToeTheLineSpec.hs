module Arkham.Asset.Assets.ToeTheLineSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetDamage))
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Modifier
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Toe the Line" do
  describe "Disavowed and Disgraced" do
    it "places 1 damage on itself when you take damage" . gameTest $ \self -> do
      toeTheLine <- self `putAssetIntoPlay` Assets.toeTheLine
      run $ InvestigatorDirectDamage (toId self) (TestSource mempty) 2 0
      applyAllDamage
      useForcedAbility
      -- "Place 1 damage" per instance of taking damage, not per point.
      field AssetDamage toeTheLine `shouldReturn` 1

    it "marks 1 progress when its damage reaches your maximum health" . gameTest $ \self -> do
      toeTheLine <- self `putAssetIntoPlay` Assets.toeTheLine
      health <- field InvestigatorHealth (toId self)
      run $ PlaceTokens GameSource (toTarget toeTheLine) #damage health
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.ToeTheLine `shouldReturn` 1

    it "marks no progress below your maximum health" . gameTest $ \self -> do
      toeTheLine <- self `putAssetIntoPlay` Assets.toeTheLine
      health <- field InvestigatorHealth (toId self)
      run $ PlaceTokens GameSource (toTarget toeTheLine) #damage (health - 1)
      endGame
      self `getActionsFrom` toeTheLine `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.ToeTheLine `shouldReturn` 0

  describe "Restored and Resolved" do
    it "grants +1 combat and +2 maximum health" . gameTest $ \self -> do
      _ <- self `putAssetIntoPlay` Assets.toeTheLineCompleted
      getModifiers self `shouldContainM` [SkillModifier #combat 1, HealthModifier 2]
