module Arkham.Treachery.Cards.DeadlyTorrent (deadlyTorrent) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeadlyTorrent = DeadlyTorrent TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadlyTorrent :: TreacheryCard DeadlyTorrent
deadlyTorrent = treachery DeadlyTorrent Cards.deadlyTorrent

-- TODO: abilities
instance RunMessage DeadlyTorrent where
  runMessage msg (DeadlyTorrent attrs) = runQueueT $ DeadlyTorrent <$> liftRunMessage msg attrs
