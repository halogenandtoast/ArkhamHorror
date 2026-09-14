module Arkham.Asset.Assets.ToeTheLineSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetDamage))
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Investigator.Cards qualified as Investigators
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

    -- Defeat flags the investigator before the elimination window opens, which
    -- used to make them unselectable and silently drop the Task (#5619). The
    -- second investigator keeps the scenario from ending on the spot.
    it "marks 1 progress when you are defeated at your maximum health" . gameTest $ \self -> do
      _ <- addInvestigator Investigators.rolandBanks
      toeTheLine <- self `putAssetIntoPlay` Assets.toeTheLine
      health <- field InvestigatorHealth (toId self)
      run $ PlaceTokens GameSource (toTarget toeTheLine) #damage health
      run $ InvestigatorIsDefeated (TestSource mempty) (toId self)
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.ToeTheLine `shouldReturn` 1

    it "marks no progress when you are defeated below your maximum health" . gameTest $ \self -> do
      _ <- addInvestigator Investigators.rolandBanks
      toeTheLine <- self `putAssetIntoPlay` Assets.toeTheLine
      health <- field InvestigatorHealth (toId self)
      run $ PlaceTokens GameSource (toTarget toeTheLine) #damage (health - 1)
      run $ InvestigatorIsDefeated (TestSource mempty) (toId self)
      getRecordCountForInvestigator (toId self) Key.ToeTheLine `shouldReturn` 0

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
