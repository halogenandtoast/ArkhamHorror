module Arkham.Treachery.Cards.AlienFoodChain (alienFoodChain) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AlienFoodChain = AlienFoodChain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienFoodChain :: TreacheryCard AlienFoodChain
alienFoodChain = treachery AlienFoodChain Cards.alienFoodChain

instance RunMessage AlienFoodChain where
  runMessage msg (AlienFoodChain attrs) = AlienFoodChain <$> runMessage msg attrs
