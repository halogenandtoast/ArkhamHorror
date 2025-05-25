module Arkham.Event.Events.NoStoneUnturned5 (noStoneUnturned5) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype NoStoneUnturned5 = NoStoneUnturned5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned5 :: EventCard NoStoneUnturned5
noStoneUnturned5 = event NoStoneUnturned5 Cards.noStoneUnturned5

instance RunMessage NoStoneUnturned5 where
  runMessage msg e@(NoStoneUnturned5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.manipulate.deck
      chooseTargetM iid iids \iid' -> search iid' attrs iid' [fromDeck] #any (DrawFound iid' 1)
      pure e
    _ -> NoStoneUnturned5 <$> liftRunMessage msg attrs
