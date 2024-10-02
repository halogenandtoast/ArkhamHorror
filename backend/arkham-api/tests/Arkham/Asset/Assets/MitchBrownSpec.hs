module Arkham.Asset.Assets.MitchBrownSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Mitch Brown" do
  it "provides 2 additional ally slots" . gameTest $ \self -> do
    assertChanges (length <$> self.slots.ally) 1 3 $ void $ self `putAssetIntoPlay` Assets.mitchBrown

  it "should not force you to discard an asset that will have room after play" . gameTest $ \self -> do
    void $ self `putAssetIntoPlay` Assets.sledDog
    void $ self `putAssetIntoPlay` Assets.mitchBrown
    peekQueue `shouldReturn` []
