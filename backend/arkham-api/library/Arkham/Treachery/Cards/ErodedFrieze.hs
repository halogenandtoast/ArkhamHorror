module Arkham.Treachery.Cards.ErodedFrieze (erodedFrieze) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ErodedFrieze = ErodedFrieze TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erodedFrieze :: TreacheryCard ErodedFrieze
erodedFrieze = treachery ErodedFrieze Cards.erodedFrieze

-- TODO: abilities
instance RunMessage ErodedFrieze where
  runMessage msg (ErodedFrieze attrs) = runQueueT $ ErodedFrieze <$> liftRunMessage msg attrs
