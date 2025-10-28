module Arkham.Asset.Assets.OccultReliquary3 (occultReliquary3) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Slot

newtype OccultReliquary3 = OccultReliquary3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultReliquary3 :: AssetCard OccultReliquary3
occultReliquary3 = asset OccultReliquary3 Cards.occultReliquary3

instance RunMessage OccultReliquary3 where
  runMessage msg (OccultReliquary3 attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid (sameCard attrs -> True) -> do
      let slot =
            AdjustableSlot (toSource attrs) (Just $ oneOf [#blessed, #cursed]) [#hand, #accessory, #arcane] []
      let addSlot sType = push $ AddSlot iid sType slot

      chooseOneM iid $ withI18n do
        questionLabeled' "cards.occultReliquary.choose"
        keyVar "slot" "slot.hand" $ labeled' "startAsSlot" $ addSlot #hand
        keyVar "slot" "slot.accessory" $ labeled' "startAsSlot" $ addSlot #accessory
        keyVar "slot" "slot.arcane" $ labeled' "startAsSlot" $ addSlot #arcane

      OccultReliquary3 <$> liftRunMessage msg attrs
    _ -> OccultReliquary3 <$> liftRunMessage msg attrs
