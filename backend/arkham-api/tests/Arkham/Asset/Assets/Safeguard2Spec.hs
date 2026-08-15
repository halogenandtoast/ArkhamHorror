module Arkham.Asset.Assets.Safeguard2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

spec :: Spec
spec = describe "Safeguard (2)" do
  context "for the remainder of that investigator's turn" do
    it "you may move to a connecting location they move to" . gameTest $ \self -> do
      roland <- addInvestigator rolandBanks
      (location1, location2) <- testConnectedLocations id id
      updateProp @"clues" 0 location1
      updateProp @"clues" 0 location2
      self `moveTo` location1
      roland `moveTo` location1
      run $ RevealLocation Nothing (toId location2)

      safeguard2 <- roland `putAssetIntoPlay` Assets.safeguard2
      [useSafeguard] <- roland `getActionsFrom` safeguard2
      roland `useAbility` useSafeguard

      pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
      [usePendant] <- self `getActionsFrom` pendantOfTheQueen
      -- location2 is the only revealed location Pendant can send you to, and
      -- moving is the only thing it can do there, so both choices auto-resolve
      self `useAbility` usePendant

      self.location `shouldReturn` Just (toId location2)
      chooseOptionMatching "move too" \case
        Label lbl _ -> lbl == "$label.moveToo"
        _ -> False
      roland.location `shouldReturn` Just (toId location2)

    it "you may not move to a location that does not connect to yours" . gameTest $ \self -> do
      roland <- addInvestigator rolandBanks
      (location1, _location2) <- testConnectedLocations id id
      location3 <- testLocation
      updateProp @"clues" 0 location1
      updateProp @"clues" 0 location3
      self `moveTo` location1
      roland `moveTo` location1
      run $ RevealLocation Nothing (toId location3)

      safeguard2 <- roland `putAssetIntoPlay` Assets.safeguard2
      [useSafeguard] <- roland `getActionsFrom` safeguard2
      roland `useAbility` useSafeguard

      pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
      [usePendant] <- self `getActionsFrom` pendantOfTheQueen
      -- location3 is the only revealed location Pendant can send you to, and
      -- moving is the only thing it can do there, so both choices auto-resolve
      self `useAbility` usePendant

      self.location `shouldReturn` Just (toId location3)
      roland.location `shouldReturn` Just (toId location1)
      questions <- toList . gameQuestion <$> getGame
      liftIO $ show questions `shouldNotContain` "moveToo"
