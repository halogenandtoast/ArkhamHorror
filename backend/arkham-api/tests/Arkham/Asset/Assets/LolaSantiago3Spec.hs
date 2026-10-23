module Arkham.Asset.Assets.LolaSantiago3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetResources))
import Arkham.Classes.HasGame (getGame)
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Lola Santiago (3)" $ do
  context "spend X resources, where X is the shroud value of your location" $ do
    {- Family Inheritance grants AsIfResourcePool, which sends the payment down the
    "which pool do you want to spend from?" path. That prompt was built without
    checking the amount, so an X of 0 asked anyway and never counted down (#5900). -}
    it "costs nothing at shroud 0, even with a second pool to spend from" . gameTest $ \self -> do
      withProp @"resources" 3 self
      location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
      self `moveTo` location
      lolaSantiago <- self `putAssetIntoPlay` Assets.lolaSantiago3
      familyInheritance <- self `putAssetIntoPlay` Assets.familyInheritance
      run $ PlaceResources (TestSource mempty) (toTarget familyInheritance) 4

      [discoverClue] <- self `getActionsFrom` lolaSantiago
      self `useAbility` discoverClue

      location.clues `shouldReturn` 0
      self.clues `shouldReturn` 1
      self.resources `shouldReturn` 3
      field AssetResources familyInheritance `shouldReturn` 4
      questions <- toList . gameQuestion <$> getGame
      liftIO $ questions `shouldBe` []

    it "still spends X from the other pool when the shroud is not 0" . gameTest $ \self -> do
      withProp @"resources" 0 self
      location <- testLocation & prop @"clues" 1 & prop @"shroud" 2
      self `moveTo` location
      lolaSantiago <- self `putAssetIntoPlay` Assets.lolaSantiago3
      familyInheritance <- self `putAssetIntoPlay` Assets.familyInheritance
      run $ PlaceResources (TestSource mempty) (toTarget familyInheritance) 4

      [discoverClue] <- self `getActionsFrom` lolaSantiago
      self `useAbility` discoverClue
      let spendFromInheritance = chooseOptionMatching "spend from Family Inheritance" \case
            TargetLabel target _ -> target == toTarget familyInheritance
            _ -> False
      spendFromInheritance
      spendFromInheritance

      location.clues `shouldReturn` 0
      self.clues `shouldReturn` 1
      field AssetResources familyInheritance `shouldReturn` 2
