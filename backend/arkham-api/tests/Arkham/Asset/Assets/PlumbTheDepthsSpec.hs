module Arkham.Asset.Assets.PlumbTheDepthsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Location.Types (Field (LocationClues), revealCluesL)
import Arkham.Modifier
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Plumb the Depths" do
  describe "Compulsive Research" do
    it "removes 1 obsession when you discover the last clue at your location" . gameTest $ \self -> do
      plumbTheDepths <- self `putAssetIntoPlay` Assets.plumbTheDepths
      -- The default test location comes in with 2 clues; the ability only
      -- triggers on discovering the *last* one.
      location <- testLocationWith (revealCluesL .~ Static 1)
      self `moveTo` location
      field LocationClues (toId location) `shouldReturn` 1
      self `discoverClues` 1
      useForcedAbility
      fieldMap AssetUses (findWithDefault 0 Obsession) plumbTheDepths `shouldReturn` 4

    it "marks 1 progress when every obsession has been removed" . gameTest $ \self -> do
      plumbTheDepths <- self `putAssetIntoPlay` Assets.plumbTheDepths
      run $ SpendUses (TestSource mempty) (toTarget plumbTheDepths) Obsession 5
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.PlumbTheDepths `shouldReturn` 1

    it "marks no progress while obsessions remain" . gameTest $ \self -> do
      plumbTheDepths <- self `putAssetIntoPlay` Assets.plumbTheDepths
      run $ SpendUses (TestSource mempty) (toTarget plumbTheDepths) Obsession 4
      endGame
      self `getActionsFrom` plumbTheDepths `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.PlumbTheDepths `shouldReturn` 0

  describe "Mastered Acumen" do
    it "grants +1 intellect" . gameTest $ \self -> do
      _ <- self `putAssetIntoPlay` Assets.plumbTheDepthsCompleted
      getModifiers self `shouldContainM` [SkillModifier #intellect 1]

    it "exhausts to draw 1 card when you discover the last clue at your location" . gameTest $ \self -> do
      _ <- self `putAssetIntoPlay` Assets.plumbTheDepthsCompleted
      self `loadDeck` [Assets.expeditionGear]
      -- The default test location comes in with 2 clues; the ability only
      -- triggers on discovering the *last* one.
      location <- testLocationWith (revealCluesL .~ Static 1)
      self `moveTo` location
      field LocationClues (toId location) `shouldReturn` 1
      self `discoverClues` 1
      useReaction
      self.hand `shouldSatisfyM` ((== 1) . length)
