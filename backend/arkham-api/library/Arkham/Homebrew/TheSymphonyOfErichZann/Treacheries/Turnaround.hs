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
    {- "Gather the [[Music]] treacheries next to the agenda deck and shuffle them.
    Then, put them into play, one by one, next to the agenda deck." The cards
    never leave play -- only the order in which the agenda's maximum will push
    them out changes, which is exactly the recorded order. -}
    Revelation _ (isSource attrs -> True) -> do
      setMusicOrder =<< shuffleM =<< getMusicOrder
      pure t
    _ -> Turnaround <$> liftRunMessage msg attrs
