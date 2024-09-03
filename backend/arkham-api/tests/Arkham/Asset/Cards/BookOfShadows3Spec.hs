module Arkham.Asset.Cards.BookOfShadows3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Book of Shadows (3)" $ do
  it "gives 1 additional arcane slot" . gameTest $ \self -> do
    self `putCardIntoPlay` Assets.bookOfShadows3
    self.arcaneSlots `shouldSatisfyM` ((== 3) . length)

  context "Action" $ do
    it "adds 1 charge to a spell asset you control" . gameTest $ \self -> do
      shrivelling <- self `putAssetIntoPlay` Assets.shrivelling
      bookOfShadows3 <- self `putAssetIntoPlay` Assets.bookOfShadows3
      [addCharge] <- self `getActionsFrom` bookOfShadows3
      self `useAbility` addCharge
      chooseTarget shrivelling
      shrivelling.charges `shouldReturn` 5
