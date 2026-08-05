module Arkham.Asset.Assets.DreamsOfDestructionSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetHorror))
import Arkham.Campaigns.TheDrownedCity.Helpers (getRecordCountForInvestigator)
import Arkham.Campaigns.TheDrownedCity.Key qualified as Key
import Arkham.Modifier
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Dreams of Destruction" do
  describe "Visions of a Grim Future" do
    it "places 1 horror on itself when you take horror" . gameTest $ \self -> do
      dreams <- self `putAssetIntoPlay` Assets.dreamsOfDestruction
      run $ InvestigatorDirectDamage (toId self) (TestSource mempty) 0 2
      applyAllHorror
      useForcedAbility
      -- "Place 1 horror" per instance of taking horror, not per point.
      field AssetHorror dreams `shouldReturn` 1

    it "marks 1 progress when its horror reaches your maximum sanity" . gameTest $ \self -> do
      dreams <- self `putAssetIntoPlay` Assets.dreamsOfDestruction
      sanity <- field InvestigatorSanity (toId self)
      run $ PlaceTokens GameSource (toTarget dreams) #horror sanity
      endGame
      useForcedAbility
      getRecordCountForInvestigator (toId self) Key.DreamsOfDestruction `shouldReturn` 1

    it "marks no progress below your maximum sanity" . gameTest $ \self -> do
      dreams <- self `putAssetIntoPlay` Assets.dreamsOfDestruction
      sanity <- field InvestigatorSanity (toId self)
      run $ PlaceTokens GameSource (toTarget dreams) #horror (sanity - 1)
      endGame
      self `getActionsFrom` dreams `shouldReturn` []
      getRecordCountForInvestigator (toId self) Key.DreamsOfDestruction `shouldReturn` 0

  describe "The Future is Not Fixed" do
    it "grants an additional arcane slot and +2 maximum sanity" . gameTest $ \self -> do
      _ <- self `putAssetIntoPlay` Assets.dreamsOfDestructionCompleted
      getModifiers self `shouldContainM` [AdditionalSlot #arcane, SanityModifier 2]
