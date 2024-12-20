module Arkham.Location.Cards.ClutteredDormitory (clutteredDormitory) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ClutteredDormitory = ClutteredDormitory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clutteredDormitory :: LocationCard ClutteredDormitory
clutteredDormitory = location ClutteredDormitory Cards.clutteredDormitory 4 (PerPlayer 2)

instance HasAbilities ClutteredDormitory where
  getAbilities (ClutteredDormitory attrs) =
    extendRevealed attrs []

instance RunMessage ClutteredDormitory where
  runMessage msg (ClutteredDormitory attrs) = runQueueT $ case msg of
    _ -> ClutteredDormitory <$> liftRunMessage msg attrs
