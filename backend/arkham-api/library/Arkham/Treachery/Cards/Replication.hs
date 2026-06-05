module Arkham.Treachery.Cards.Replication (replication) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Replication = Replication TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

replication :: TreacheryCard Replication
replication = treachery Replication Cards.replication

instance RunMessage Replication where
  runMessage msg (Replication attrs) = Replication <$> runMessage msg attrs
