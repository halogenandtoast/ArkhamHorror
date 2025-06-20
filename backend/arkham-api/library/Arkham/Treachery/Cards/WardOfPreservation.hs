module Arkham.Treachery.Cards.WardOfPreservation (wardOfPreservation) where

import Arkham.Ability
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WardOfPreservation = WardOfPreservation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wardOfPreservation :: TreacheryCard WardOfPreservation
wardOfPreservation = treachery WardOfPreservation Cards.wardOfPreservation

instance HasAbilities WardOfPreservation where
  getAbilities (WardOfPreservation a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ PlayEvent #when You AnyEvent]

instance RunMessage WardOfPreservation where
  runMessage msg t@(WardOfPreservation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (getPlayedEvent -> eventId) _ -> do
      cancelEvent eventId
      assignHorror iid (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WardOfPreservation <$> liftRunMessage msg attrs
