module Arkham.Treachery.Cards.HangingOnTheEdge (hangingOnTheEdge) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HangingOnTheEdge = HangingOnTheEdge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hangingOnTheEdge :: TreacheryCard HangingOnTheEdge
hangingOnTheEdge = treachery HangingOnTheEdge Cards.hangingOnTheEdge

instance RunMessage HangingOnTheEdge where
  runMessage msg t@(HangingOnTheEdge attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> HangingOnTheEdge <$> liftRunMessage msg attrs
