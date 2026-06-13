module Arkham.Location.Cards.AncientGallery (ancientGallery) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AncientGallery = AncientGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientGallery :: LocationCard AncientGallery
ancientGallery = location AncientGallery Cards.ancientGallery 3 (Static 3)

-- TODO: abilities

instance RunMessage AncientGallery where
  runMessage msg (AncientGallery attrs) = runQueueT $ AncientGallery <$> liftRunMessage msg attrs
