module Arkham.Treachery.Cards.CorrosiveFog (corrosiveFog) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CorrosiveFog = CorrosiveFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosiveFog :: TreacheryCard CorrosiveFog
corrosiveFog = treachery CorrosiveFog Cards.corrosiveFog

-- TODO: abilities
instance RunMessage CorrosiveFog where
  runMessage msg (CorrosiveFog attrs) = runQueueT $ CorrosiveFog <$> liftRunMessage msg attrs
