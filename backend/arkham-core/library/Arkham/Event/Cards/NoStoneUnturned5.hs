module Arkham.Event.Cards.NoStoneUnturned5
  ( noStoneUnturned5
  , NoStoneUnturned5(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype NoStoneUnturned5 = NoStoneUnturned5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noStoneUnturned5 :: EventCard NoStoneUnturned5
noStoneUnturned5 =
  event NoStoneUnturned5 Cards.noStoneUnturned5

instance RunMessage NoStoneUnturned5 where
  runMessage msg e@(NoStoneUnturned5 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> NoStoneUnturned5 <$> runMessage msg attrs
