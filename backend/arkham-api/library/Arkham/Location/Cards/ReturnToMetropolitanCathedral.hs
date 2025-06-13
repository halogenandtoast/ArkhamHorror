module Arkham.Location.Cards.ReturnToMetropolitanCathedral (returnToMetropolitanCathedral) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ReturnToMetropolitanCathedral = ReturnToMetropolitanCathedral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToMetropolitanCathedral :: LocationCard ReturnToMetropolitanCathedral
returnToMetropolitanCathedral = symbolLabel $ location ReturnToMetropolitanCathedral Cards.returnToMetropolitanCathedral 4 (Static 0)

instance HasAbilities ReturnToMetropolitanCathedral where
  getAbilities (ReturnToMetropolitanCathedral attrs) =
    extendRevealed attrs []

instance RunMessage ReturnToMetropolitanCathedral where
  runMessage msg (ReturnToMetropolitanCathedral attrs) = runQueueT $ case msg of
    _ -> ReturnToMetropolitanCathedral <$> liftRunMessage msg attrs
