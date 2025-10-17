module Arkham.Location.Cards.TheCabildo (theCabildo) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheCabildo = TheCabildo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCabildo :: LocationCard TheCabildo
theCabildo = setLabel "e" $ location TheCabildo Cards.theCabildo 4 (Static 2)

instance HasAbilities TheCabildo where
  getAbilities (TheCabildo attrs) =
    extendRevealed attrs []

instance RunMessage TheCabildo where
  runMessage msg (TheCabildo attrs) = runQueueT $ case msg of
    _ -> TheCabildo <$> liftRunMessage msg attrs
