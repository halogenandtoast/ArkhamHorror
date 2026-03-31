module Arkham.Asset.Assets.ArcaneExperience4 (arcaneExperience4) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Slot

newtype ArcaneExperience4 = ArcaneExperience4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneExperience4 :: AssetCard ArcaneExperience4
arcaneExperience4 = asset ArcaneExperience4 Cards.arcaneExperience4

instance RunMessage ArcaneExperience4 where
  runMessage msg (ArcaneExperience4 attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      push $ AddSlot iid #arcane (Slot (toSource attrs) [])
      ArcaneExperience4 <$> liftRunMessage msg attrs
    _ -> ArcaneExperience4 <$> liftRunMessage msg attrs
