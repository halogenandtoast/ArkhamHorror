module Arkham.Asset.Cards.PeterSylvestreSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Peter Sylvestre" $ do
  gives @"agility" Assets.peterSylvestre 1

  it "removes one horror at the end of your turn" . gameTest $ \self -> do
    peterSylvestre <- self `putAssetIntoPlay` Assets.peterSylvestre
    duringTurn self $ do
      run $ AssetDamage peterSylvestre (TestSource mempty) 0 1

    assertChanges peterSylvestre.horror 1 0 useReaction
