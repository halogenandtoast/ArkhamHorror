module Arkham.Treachery.Cards.Swarm (swarm) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Swarm = Swarm TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swarm :: TreacheryCard Swarm
swarm = treachery Swarm Cards.swarm

instance RunMessage Swarm where
  runMessage msg t@(Swarm attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Swarm <$> liftRunMessage msg attrs
