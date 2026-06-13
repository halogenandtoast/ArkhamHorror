module Arkham.Treachery.Cards.GrossPlasticity (grossPlasticity) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GrossPlasticity = GrossPlasticity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grossPlasticity :: TreacheryCard GrossPlasticity
grossPlasticity = treachery GrossPlasticity Cards.grossPlasticity

-- TODO: abilities
instance RunMessage GrossPlasticity where
  runMessage msg (GrossPlasticity attrs) = runQueueT $ GrossPlasticity <$> liftRunMessage msg attrs
