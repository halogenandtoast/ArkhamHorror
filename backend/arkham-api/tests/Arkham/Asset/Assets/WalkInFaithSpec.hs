module Arkham.Asset.Assets.WalkInFaithSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Modifier
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Walk in Faith" do
  describe "Blind Devotion" do
    it "places 1 sign when an elder sign is revealed" . gameTest $ \self -> do
      walkInFaith <- self `putAssetIntoPlay` Assets.walkInFaith
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #willpower 0
      useReaction
      fieldMap AssetUses (findWithDefault 0 Sign) walkInFaith `shouldReturn` 1

    it "marks 1 progress when the game ends with more than 1 per investigator signs" . gameTest $ \self -> do
      walkInFaith <- self `putAssetIntoPlay` Assets.walkInFaith
      run $ PlaceTokens GameSource (toTarget walkInFaith) Sign 2
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.WalkInFaith `shouldReturn` 1

    it "marks no progress at exactly 1 per investigator sign" . gameTest $ \self -> do
      walkInFaith <- self `putAssetIntoPlay` Assets.walkInFaith
      run $ PlaceTokens GameSource (toTarget walkInFaith) Sign 1
      endGame
      self `getActionsFrom` walkInFaith `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.WalkInFaith `shouldReturn` 0

  describe "Filled with Spirit" do
    it "grants +1 willpower" . gameTest $ \self -> do
      _ <- self `putAssetIntoPlay` Assets.walkInFaithCompleted
      getModifiers self `shouldContainM` [SkillModifier #willpower 1]

    -- The ability heals "an investigator at that location", so the reaction only
    -- exists once the revealing investigator actually has a location.
    it "heals 1 horror when an elder sign is revealed" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      _ <- self `putAssetIntoPlay` Assets.walkInFaithCompleted
      self `addHorror` 2
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #willpower 0
      useReaction
      self.horror `shouldReturn` 1
