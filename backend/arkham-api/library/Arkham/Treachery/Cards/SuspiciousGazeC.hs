module Arkham.Treachery.Cards.SuspiciousGazeC (suspiciousGazeC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SuspiciousGazeC = SuspiciousGazeC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiciousGazeC :: TreacheryCard SuspiciousGazeC
suspiciousGazeC = treachery SuspiciousGazeC Cards.suspiciousGazeC

instance RunMessage SuspiciousGazeC where
  runMessage msg t@(SuspiciousGazeC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SuspiciousGazeC <$> liftRunMessage msg attrs
