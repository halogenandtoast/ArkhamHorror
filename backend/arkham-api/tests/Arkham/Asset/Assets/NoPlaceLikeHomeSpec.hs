module Arkham.Asset.Assets.NoPlaceLikeHomeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Location.Types (revealedL)
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "No Place Like Home" do
  it "removes 1 discovery when you reveal a location" . gameTest $ \self -> do
    location <- testLocationWith (revealedL .~ False)
    nplh <- self `putAssetIntoPlay` Assets.noPlaceLikeHome
    run $ RevealLocation (Just $ toId self) (toId location)
    fieldMap AssetUses (findWithDefault 0 Discovery) nplh `shouldReturn` 7
