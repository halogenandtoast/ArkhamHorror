module Arkham.Treachery.Cards.EmpyreanBrilliance (empyreanBrilliance) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EmpyreanBrilliance = EmpyreanBrilliance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empyreanBrilliance :: TreacheryCard EmpyreanBrilliance
empyreanBrilliance = treachery EmpyreanBrilliance Cards.empyreanBrilliance

instance RunMessage EmpyreanBrilliance where
  runMessage msg t@(EmpyreanBrilliance attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EmpyreanBrilliance <$> liftRunMessage msg attrs
