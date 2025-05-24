module Arkham.Event.Events.NoStoneUnturned (noStoneUnturned) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype NoStoneUnturned = NoStoneUnturned EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned :: EventCard NoStoneUnturned
noStoneUnturned = event NoStoneUnturned Cards.noStoneUnturned

instance RunMessage NoStoneUnturned where
  runMessage msg e@(NoStoneUnturned attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.manipulate.deck
      chooseTargetM iid iids \iid' -> search iid' attrs iid' [fromTopOfDeck 6] #any (DrawFound iid' 1)
      pure e
    _ -> NoStoneUnturned <$> liftRunMessage msg attrs
