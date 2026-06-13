module Arkham.Location.Cards.TreacherousPathErodedShelf (treacherousPathErodedShelf) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TreacherousPathErodedShelf = TreacherousPathErodedShelf LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathErodedShelf :: LocationCard TreacherousPathErodedShelf
treacherousPathErodedShelf = location TreacherousPathErodedShelf Cards.treacherousPathErodedShelf 0 (Static 1)

-- TODO: abilities

instance RunMessage TreacherousPathErodedShelf where
  runMessage msg (TreacherousPathErodedShelf attrs) = runQueueT $ TreacherousPathErodedShelf <$> liftRunMessage msg attrs
