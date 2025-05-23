module Arkham.Event.Events.ImOuttaHere (imOuttaHere) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype ImOuttaHere = ImOuttaHere EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imOuttaHere :: EventCard ImOuttaHere
imOuttaHere = event ImOuttaHere Cards.imOuttaHere

instance RunMessage ImOuttaHere where
  runMessage msg e@(ImOuttaHere attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      resign iid
      pure e
    _ -> ImOuttaHere <$> liftRunMessage msg attrs
