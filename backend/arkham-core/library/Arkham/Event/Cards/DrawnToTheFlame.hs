module Arkham.Event.Cards.DrawnToTheFlame where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message

newtype DrawnToTheFlame = DrawnToTheFlame EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drawnToTheFlame :: EventCard DrawnToTheFlame
drawnToTheFlame = event DrawnToTheFlame Cards.drawnToTheFlame

instance RunMessage DrawnToTheFlame where
  runMessage msg e@(DrawnToTheFlame attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      pushAll
        [ InvestigatorDrawEncounterCard iid
        , InvestigatorDiscoverCluesAtTheirLocation iid (toSource attrs) 2 Nothing
        ]
      pure e
    _ -> DrawnToTheFlame <$> runMessage msg attrs
