module Arkham.Investigator.Cards.TommyMuldoonSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (tommyMuldoon)
import TestImport.New

spec :: Spec
spec = describe "Tommy Muldoon" $ do
  context "When an asset you control is defeated" $ do
    it "gain x resources, where x is the total damage and horror on that asset" . gameTestWith tommyMuldoon $ \self -> do
      beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
      run $ AssetDamage beatCop2 (TestSource mempty) 3 1
      useReaction
      self.resources `shouldReturn` 4

    it "that asset is shuffled into your deck" . gameTestWith tommyMuldoon $ \self -> do
      beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
      run $ AssetDamage beatCop2 (TestSource mempty) 3 1
      useReaction
      self.discard `shouldReturn` []
      asDefs self.deck `shouldReturn` [Assets.beatCop2]

  context "elder sign effect" $ do
    it "You may move up to 2 damage and/or horror from Tommy Muldoon to an asset you control" . gameTestWith tommyMuldoon $ \self -> do
      withProp @"damage" 1 self
      withProp @"horror" 1 self
      beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
      setChaosTokens [ElderSign]
      runSkillTest self #agility 100
      chooseFirstOption "Move up to 2 damage and/or horror from Tommy Muldoon to an asset you control"
      chooseOptionMatching "move horror to asset" \case
        ComponentLabel (AssetComponent _ HorrorToken) _ -> True
        _ -> False
      chooseOptionMatching "move damage to asset" \case
        ComponentLabel (AssetComponent _ DamageToken) _ -> True
        _ -> False

      self.damage `shouldReturn` 0
      self.horror `shouldReturn` 0
      beatCop2.damage `shouldReturn` 1
      beatCop2.horror `shouldReturn` 1

    it "or vice versa" . gameTestWith tommyMuldoon $ \self -> do
      beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
      run $ AssetDamage beatCop2 (TestSource mempty) 2 1
      setChaosTokens [ElderSign]
      runSkillTest self #agility 100
      chooseFirstOption "Move up to 2 damage and/or horror from an asset you control to Tommy Muldoon"
      chooseOptionMatching "move horror to tommy muldoon" \case
        ComponentLabel (AssetComponent _ HorrorToken) _ -> True
        _ -> False
      chooseOptionMatching "move damage to tommy muldoon" \case
        ComponentLabel (AssetComponent _ DamageToken) _ -> True
        _ -> False

      self.damage `shouldReturn` 1
      self.horror `shouldReturn` 1
      beatCop2.damage `shouldReturn` 1
      beatCop2.horror `shouldReturn` 0
