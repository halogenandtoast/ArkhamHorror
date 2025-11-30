module Arkham.Treachery.Cards.LocusPulse (locusPulse) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LocusPulse = LocusPulse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

locusPulse :: TreacheryCard LocusPulse
locusPulse = treachery LocusPulse Cards.locusPulse

instance RunMessage LocusPulse where
  runMessage msg t@(LocusPulse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> LocusPulse <$> liftRunMessage msg attrs
