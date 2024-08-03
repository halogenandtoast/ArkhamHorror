module Arkham.Asset.Cards.OccultReliquary3 (occultReliquary3, OccultReliquary3 (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Slot

newtype OccultReliquary3 = OccultReliquary3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultReliquary3 :: AssetCard OccultReliquary3
occultReliquary3 = asset OccultReliquary3 Cards.occultReliquary3

instance RunMessage OccultReliquary3 where
  runMessage msg (OccultReliquary3 attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      let addSlot sType =
            AddSlot iid sType
              $ AdjustableSlot (toSource attrs) (Just $ oneOf [#blessed, #cursed]) [#hand, #accessory, #arcane] []

      questionLabel "Choose initial slot type for Occult Reliquary" iid
        $ ChooseOne
          [ Label "Start as hand slot" [addSlot #hand]
          , Label "Start as accessory slot" [addSlot #accessory]
          , Label "Start as arcane slot" [addSlot #arcane]
          ]
      OccultReliquary3 <$> runMessage msg attrs
    _ -> OccultReliquary3 <$> liftRunMessage msg attrs
