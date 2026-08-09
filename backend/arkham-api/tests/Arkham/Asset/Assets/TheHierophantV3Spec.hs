module Arkham.Asset.Assets.TheHierophantV3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Slot
import Arkham.Matcher
import Arkham.Projection
import TestImport.Lifted

spec :: Spec
spec = describe "The Hierophant V (3)" $ do
  it "still refills slots with two copies in play" $ gameTest $ \self -> do
    -- #5363: Moon Pendant takes the only accessory slot and grants the second tarot slot,
    -- which is how two copies of The Hierophant reach play at once. Each copy contributes
    -- the same SlotCanBe modifiers, and that duplicate pair used to crash RefillSlots when
    -- an accessory-slot asset had to fall back to an arcane slot.
    putCardIntoPlay self Assets.moonPendant2
    putCardIntoPlay self Assets.theHierophantV3
    putCardIntoPlay self Assets.theHierophantV3
    putCardIntoPlay self Assets.holyRosary
    run $ RefillSlots (toId self) []

    -- Both copies must really be in play, otherwise the duplicated modifier -- the whole
    -- point of this test -- never happens.
    hierophants <- select $ assetIs Assets.theHierophantV3
    length hierophants `shouldBe` 2

    -- Only one accessory slot exists for two accessory-slot assets, so one of them has to
    -- borrow an arcane slot. Either assignment is fine; both must end up slotted.
    rosary <- selectJust $ assetIs Assets.holyRosary
    pendant <- selectJust $ assetIs Assets.moonPendant2
    slots <- field InvestigatorSlots (toId self)
    let occupants = concatMap slotItems $ concat $ toList slots
    occupants `shouldContain` [rosary]
    occupants `shouldContain` [pendant]
