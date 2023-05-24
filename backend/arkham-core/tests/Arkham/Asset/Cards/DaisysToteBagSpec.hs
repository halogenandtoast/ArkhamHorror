module Arkham.Asset.Cards.DaisysToteBagSpec (
  spec,
)
where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher (assetIs)
import Arkham.Projection
import Arkham.Trait (Trait (Tome))

spec :: Spec
spec = describe "Daisy's Tote Bag" $ do
  it "Grants 2 additional hand slots, which can only be used to hold tome assets" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
    putCardIntoPlay daisyWalker Assets.daisysToteBag
    let
      matchingSlot = \case
        TraitRestrictedSlot _ Tome _ -> True
        _ -> False
    assert $
      fieldP
        InvestigatorSlots
        ((== 2) . count matchingSlot . findWithDefault [] HandSlot)
        (toId daisyWalker)

  it "those slots are lost if the tote bag is discarded" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
    putCardIntoPlay daisyWalker Assets.daisysToteBag
    daisysToteBag <- selectJust $ assetIs Assets.daisysToteBag
    pushAndRun $ Discard GameSource (toTarget daisysToteBag)
    let
      matchingSlot = \case
        TraitRestrictedSlot _ Tome _ -> True
        _ -> False
    assert $
      fieldP
        InvestigatorSlots
        ((== 0) . count matchingSlot . findWithDefault [] HandSlot)
        (toId daisyWalker)
