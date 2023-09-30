module Arkham.Asset.Cards.DaisysToteBagSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards
import Arkham.Matcher (assetIs)
import Arkham.Trait (Trait (Tome))
import TestImport.New

spec :: Spec
spec = describe "Daisy's Tote Bag" $ do
  let
    matchingSlotCount =
      count
        \case
          TraitRestrictedSlot _ Tome _ -> True
          _ -> False
        . findWithDefault [] #hand

  it "Grants 2 additional hand slots, which can only be used to hold tome assets" . gameTestWith daisyWalker $ \self -> do
    self `putCardIntoPlay` Assets.daisysToteBag
    self.slots `shouldSatisfyM` ((== 2) . matchingSlotCount)

  it "those slots are lost if the tote bag is discarded" . gameTestWith daisyWalker $ \self -> do
    self `putCardIntoPlay` Assets.daisysToteBag
    daisysToteBag <- selectJust $ assetIs Assets.daisysToteBag
    discard daisysToteBag
    self.slots `shouldSatisfyM` ((== 0) . matchingSlotCount)
