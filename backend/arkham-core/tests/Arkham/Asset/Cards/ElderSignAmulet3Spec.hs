module Arkham.Asset.Cards.ElderSignAmulet3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Elder Sign Amulet (3)" $ do
  it "has 4 sanity" . gameTest $ \self -> do
    elderSignAmulet3 <- self `putAssetIntoPlay` Assets.elderSignAmulet3
    elderSignAmulet3.sanity `shouldReturn` Just 4
