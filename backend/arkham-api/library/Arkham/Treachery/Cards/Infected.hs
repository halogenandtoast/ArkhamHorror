module Arkham.Treachery.Cards.Infected (infected) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Infected = Infected TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infected :: TreacheryCard Infected
infected = treachery Infected Cards.infected

-- TODO: abilities
instance RunMessage Infected where
  runMessage msg (Infected attrs) = runQueueT $ Infected <$> liftRunMessage msg attrs
