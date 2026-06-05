module Arkham.Treachery.Cards.ConsumingMaw (consumingMaw) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ConsumingMaw = ConsumingMaw TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

consumingMaw :: TreacheryCard ConsumingMaw
consumingMaw = treachery ConsumingMaw Cards.consumingMaw

instance RunMessage ConsumingMaw where
  runMessage msg (ConsumingMaw attrs) = ConsumingMaw <$> runMessage msg attrs
