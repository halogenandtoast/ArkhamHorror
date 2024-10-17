module Arkham.Location.Cards.TheMoonRoom
  ( theMoonRoom
  , TheMoonRoom(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheMoonRoom = TheMoonRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMoonRoom :: LocationCard TheMoonRoom
theMoonRoom = location TheMoonRoom Cards.theMoonRoom 0 (Static 0)

instance HasAbilities TheMoonRoom where
  getAbilities (TheMoonRoom attrs) =
    extendRevealed attrs []

instance RunMessage TheMoonRoom where
  runMessage msg (TheMoonRoom attrs) = runQueueT $ case msg of
    _ -> TheMoonRoom <$> liftRunMessage msg attrs
