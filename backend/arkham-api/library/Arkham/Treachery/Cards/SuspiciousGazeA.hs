module Arkham.Treachery.Cards.SuspiciousGazeA (suspiciousGazeA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SuspiciousGazeA = SuspiciousGazeA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

suspiciousGazeA :: TreacheryCard SuspiciousGazeA
suspiciousGazeA = treachery SuspiciousGazeA Cards.suspiciousGazeA

instance RunMessage SuspiciousGazeA where
  runMessage msg t@(SuspiciousGazeA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SuspiciousGazeA <$> liftRunMessage msg attrs
