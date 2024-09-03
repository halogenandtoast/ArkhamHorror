module Arkham.Asset.Cards.BulletproofVest3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Bulletproof Vest (3)" $ do
  it "has 4 health" . gameTest $ \self -> do
    bulletproofVest3 <- self `putAssetIntoPlay` Assets.bulletproofVest3
    bulletproofVest3.health `shouldReturn` Just 4
