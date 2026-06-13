module Arkham.Treachery.Cards.AlienEggs (alienEggs) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AlienEggs = AlienEggs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienEggs :: TreacheryCard AlienEggs
alienEggs = treachery AlienEggs Cards.alienEggs

-- TODO: abilities
instance RunMessage AlienEggs where
  runMessage msg (AlienEggs attrs) = runQueueT $ AlienEggs <$> liftRunMessage msg attrs
