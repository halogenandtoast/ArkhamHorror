module Arkham.Asset.Cards.LeatherCoatSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Leather Coat" $ do
  it "has 2 health" . gameTest $ \self -> do
    leatherCoat <- self `putAssetIntoPlay` Assets.leatherCoat
    leatherCoat.health `shouldReturn` Just 2
