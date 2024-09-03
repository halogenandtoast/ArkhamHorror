module Arkham.Asset.Cards.RabbitsFootSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Rabbit's Foot" $ do
  context "After you fail a skill test" $ do
    it "exhaust, draw 1 card" . gameTest $ \self -> do
      cards <- testPlayerCards 1
      withProp @"deck" (Deck cards) self
      rabbitsFoot <- self `putAssetIntoPlay` Assets.rabbitsFoot
      failSkillTest self
      useReaction
      self.hand `shouldReturn` map toCard cards
      assert rabbitsFoot.exhausted
