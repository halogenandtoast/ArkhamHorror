module Arkham.Event.Cards.NoStoneUnturned (noStoneUnturned, NoStoneUnturned (..)) where

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype NoStoneUnturned = NoStoneUnturned EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned :: EventCard NoStoneUnturned
noStoneUnturned = event NoStoneUnturned Cards.noStoneUnturned

instance RunMessage NoStoneUnturned where
  runMessage msg e@(NoStoneUnturned attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.manipulate.deck
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel iid' [search iid' attrs iid' [fromTopOfDeck 6] #any (DrawFound iid' 1)]
          | iid' <- iids
          ]
      pure e
    _ -> NoStoneUnturned <$> runMessage msg attrs
