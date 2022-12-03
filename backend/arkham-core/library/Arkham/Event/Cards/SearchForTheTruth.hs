module Arkham.Event.Cards.SearchForTheTruth where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field(..) )
import Arkham.Message
import Arkham.Projection

newtype SearchForTheTruth = SearchForTheTruth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruth :: EventCard SearchForTheTruth
searchForTheTruth = event SearchForTheTruth Cards.searchForTheTruth

instance RunMessage SearchForTheTruth where
  runMessage msg e@(SearchForTheTruth attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      clueCount' <- field InvestigatorClues iid
      e <$ pushAll
        [drawCards iid attrs (min 5 clueCount'), Discard (toTarget attrs)]
    _ -> SearchForTheTruth <$> runMessage msg attrs
