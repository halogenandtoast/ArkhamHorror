module Arkham.Event.Cards.CrypticResearch4 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype CrypticResearch4 = CrypticResearch4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticResearch4 :: EventCard CrypticResearch4
crypticResearch4 = event CrypticResearch4 Cards.crypticResearch4

instance RunMessage CrypticResearch4 where
  runMessage msg e@(CrypticResearch4 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      iids <- selectList $ colocatedWith iid
      pushAll
        [ chooseOne iid
          $ [ targetLabel iid' [drawCards iid' attrs 3] | iid' <- iids ]
        , Discard (EventTarget eid)
        ]
      pure e
    _ -> CrypticResearch4 <$> runMessage msg attrs
