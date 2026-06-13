module Arkham.Treachery.Cards.OppressiveInfluence (oppressiveInfluence) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OppressiveInfluence = OppressiveInfluence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oppressiveInfluence :: TreacheryCard OppressiveInfluence
oppressiveInfluence = treachery OppressiveInfluence Cards.oppressiveInfluence

-- TODO: abilities
instance RunMessage OppressiveInfluence where
  runMessage msg (OppressiveInfluence attrs) = runQueueT $ OppressiveInfluence <$> liftRunMessage msg attrs
