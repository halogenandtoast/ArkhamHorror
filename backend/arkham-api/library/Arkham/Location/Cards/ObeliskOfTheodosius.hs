module Arkham.Location.Cards.ObeliskOfTheodosius (obeliskOfTheodosius) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ObeliskOfTheodosius = ObeliskOfTheodosius LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obeliskOfTheodosius :: LocationCard ObeliskOfTheodosius
obeliskOfTheodosius = symbolLabel $ location ObeliskOfTheodosius Cards.obeliskOfTheodosius 3 (PerPlayer 1)

instance HasAbilities ObeliskOfTheodosius where
  getAbilities (ObeliskOfTheodosius attrs) =
    extendRevealed attrs []

instance RunMessage ObeliskOfTheodosius where
  runMessage msg (ObeliskOfTheodosius attrs) = runQueueT $ case msg of
    _ -> ObeliskOfTheodosius <$> liftRunMessage msg attrs
