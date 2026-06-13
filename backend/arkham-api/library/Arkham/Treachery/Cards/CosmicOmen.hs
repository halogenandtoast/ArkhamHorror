module Arkham.Treachery.Cards.CosmicOmen (cosmicOmen) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CosmicOmen = CosmicOmen TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicOmen :: TreacheryCard CosmicOmen
cosmicOmen = treachery CosmicOmen Cards.cosmicOmen

-- TODO: abilities
instance RunMessage CosmicOmen where
  runMessage msg (CosmicOmen attrs) = runQueueT $ CosmicOmen <$> liftRunMessage msg attrs
