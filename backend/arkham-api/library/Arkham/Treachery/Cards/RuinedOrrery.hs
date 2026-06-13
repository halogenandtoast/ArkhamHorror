module Arkham.Treachery.Cards.RuinedOrrery (ruinedOrrery) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RuinedOrrery = RuinedOrrery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinedOrrery :: TreacheryCard RuinedOrrery
ruinedOrrery = treachery RuinedOrrery Cards.ruinedOrrery

-- TODO: abilities
instance RunMessage RuinedOrrery where
  runMessage msg (RuinedOrrery attrs) = runQueueT $ RuinedOrrery <$> liftRunMessage msg attrs
