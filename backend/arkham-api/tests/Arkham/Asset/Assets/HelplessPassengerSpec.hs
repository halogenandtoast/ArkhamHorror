module Arkham.Asset.Assets.HelplessPassengerSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Helpless Passenger" do
  context "Forced - If Helpless Passenger leaves play: Each investigator takes 1 horror" do
    it "is not a legal soak target for the horror it deals on its way out" . gameTest $ \self -> do
      passenger <- self `putAssetIntoPlay` Assets.helplessPassenger
      run $ Discard Nothing GameSource (AssetTarget passenger)
      useForcedAbility
      -- The passenger is mid-removal, so the investigator is the only place the
      -- horror can land. Soaking it re-defeated the passenger and re-triggered
      -- this same ability, #5551
      chooseOnlyOption "assign the horror"
      self.horror `shouldReturn` 1

    it "still lets another ally soak the horror" . gameTest $ \self -> do
      beatCop <- self `putAssetIntoPlay` Assets.beatCop2
      passenger <- self `putAssetIntoPlay` Assets.helplessPassenger
      run $ Discard Nothing GameSource (AssetTarget passenger)
      useForcedAbility
      assignDamageTo self beatCop
      beatCop.horror `shouldReturn` 1
      self.horror `shouldReturn` 0
