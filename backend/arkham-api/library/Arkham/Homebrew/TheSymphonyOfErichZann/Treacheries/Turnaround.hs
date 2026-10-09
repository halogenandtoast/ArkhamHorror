module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.Turnaround (turnaround) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (getMusicOrder, setMusicOrder)
import Arkham.Treachery.Import.Lifted

newtype Turnaround = Turnaround TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

turnaround :: TreacheryCard Turnaround
turnaround = treachery Turnaround Cards.turnaround

instance RunMessage Turnaround where
  runMessage msg t@(Turnaround attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      setMusicOrder =<< shuffle =<< getMusicOrder
      pure t
    _ -> Turnaround <$> liftRunMessage msg attrs
