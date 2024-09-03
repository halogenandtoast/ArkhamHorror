module Arkham.Event.Cards.NoStoneUnturned5 (noStoneUnturned5, NoStoneUnturned5 (..)) where

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype NoStoneUnturned5 = NoStoneUnturned5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned5 :: EventCard NoStoneUnturned5
noStoneUnturned5 = event NoStoneUnturned5 Cards.noStoneUnturned5

instance RunMessage NoStoneUnturned5 where
  runMessage msg e@(NoStoneUnturned5 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.manipulate.deck
      player <- getPlayer iid
      pushAll
        [ chooseOne
            player
            [ targetLabel iid' [search iid' attrs iid' [fromDeck] #any (DrawFound iid' 1)]
            | iid' <- iids
            ]
        ]
      pure e
    _ -> NoStoneUnturned5 <$> runMessage msg attrs
