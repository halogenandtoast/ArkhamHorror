module Arkham.Treachery.Cards.SuspiciousGazeB (suspiciousGazeB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SuspiciousGazeB = SuspiciousGazeB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiciousGazeB :: TreacheryCard SuspiciousGazeB
suspiciousGazeB = treachery SuspiciousGazeB Cards.suspiciousGazeB

instance RunMessage SuspiciousGazeB where
  runMessage msg t@(SuspiciousGazeB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SuspiciousGazeB <$> liftRunMessage msg attrs
