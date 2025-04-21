module Arkham.Asset.Assets.ArcaneEnlightenment (arcaneEnlightenment) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Trait
import Arkham.Slot

newtype ArcaneEnlightenment = ArcaneEnlightenment AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneEnlightenment :: AssetCard ArcaneEnlightenment
arcaneEnlightenment = asset ArcaneEnlightenment Cards.arcaneEnlightenment

instance HasModifiersFor ArcaneEnlightenment where
  getModifiersFor (ArcaneEnlightenment a) = controllerGets a [HandSize 1]

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome []

instance RunMessage ArcaneEnlightenment where
  runMessage msg (ArcaneEnlightenment attrs) = runQueueT $ case msg of
    -- Slots need to be added before the asset is played so we hook into played card
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid HandSlot (slot attrs)
      ArcaneEnlightenment <$> liftRunMessage msg attrs
    _ -> ArcaneEnlightenment <$> liftRunMessage msg attrs
