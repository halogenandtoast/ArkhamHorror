module Arkham.Asset.Assets.DaisysToteBag (daisysToteBag) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Slot
import Arkham.Trait

newtype DaisysToteBag = DaisysToteBag AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBag :: AssetCard DaisysToteBag
daisysToteBag = asset DaisysToteBag Cards.daisysToteBag

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome []

instance RunMessage DaisysToteBag where
  runMessage msg (DaisysToteBag attrs) = runQueueT $ case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      pushAll $ replicate 2 (AddSlot iid #hand (slot attrs))
      DaisysToteBag <$> liftRunMessage msg attrs
    _ -> DaisysToteBag <$> liftRunMessage msg attrs
