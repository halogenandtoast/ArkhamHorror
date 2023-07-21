module Arkham.Event.Cards.WillToSurvive3 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message

newtype WillToSurvive3 = WillToSurvive3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive3 :: EventCard WillToSurvive3
willToSurvive3 = event WillToSurvive3 Cards.willToSurvive3

instance RunMessage WillToSurvive3 where
  runMessage msg e@(WillToSurvive3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      e
        <$ pushAll
          [ CreateEffect "01085" Nothing (toSource attrs) (InvestigatorTarget iid)
          ]
    _ -> WillToSurvive3 <$> runMessage msg attrs
