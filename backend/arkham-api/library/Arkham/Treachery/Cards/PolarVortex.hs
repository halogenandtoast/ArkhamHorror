module Arkham.Treachery.Cards.PolarVortex ( polarVortex , PolarVortex(..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PolarVortex = PolarVortex TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

polarVortex :: TreacheryCard PolarVortex
polarVortex = treachery PolarVortex Cards.polarVortex

instance RunMessage PolarVortex where
  runMessage msg t@(PolarVortex attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> PolarVortex <$> liftRunMessage msg attrs
