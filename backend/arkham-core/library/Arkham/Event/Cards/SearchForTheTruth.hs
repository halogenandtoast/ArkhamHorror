module Arkham.Event.Cards.SearchForTheTruth where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Id
import Arkham.Message
import Arkham.Query

newtype SearchForTheTruth = SearchForTheTruth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruth :: EventCard SearchForTheTruth
searchForTheTruth = event SearchForTheTruth Cards.searchForTheTruth

instance (HasQueue env, HasCount ClueCount env InvestigatorId) => RunMessage SearchForTheTruth where
  runMessage msg e@(SearchForTheTruth attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      clueCount' <- unClueCount <$> getCount iid
      e <$ pushAll
        [DrawCards iid (min 5 clueCount') False, Discard (toTarget attrs)]
    _ -> SearchForTheTruth <$> runMessage msg attrs
