module Arkham.Location.Cards.TheThroneRoom (theThroneRoom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheThroneRoom = TheThroneRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThroneRoom :: LocationCard TheThroneRoom
theThroneRoom = location TheThroneRoom Cards.theThroneRoom 2 (PerPlayer 2)

instance HasAbilities TheThroneRoom where
  getAbilities (TheThroneRoom attrs) =
    extendRevealed attrs []

instance RunMessage TheThroneRoom where
  runMessage msg (TheThroneRoom attrs) = runQueueT $ case msg of
    _ -> TheThroneRoom <$> liftRunMessage msg attrs
