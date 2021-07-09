module Arkham.Types.Event.Cards.SearchForTheTruth where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query

newtype SearchForTheTruth = SearchForTheTruth EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruth :: EventCard SearchForTheTruth
searchForTheTruth = event SearchForTheTruth Cards.searchForTheTruth

instance HasModifiersFor env SearchForTheTruth where
  getModifiersFor = noModifiersFor

instance HasActions env SearchForTheTruth where
  getActions i window (SearchForTheTruth attrs) = getActions i window attrs

instance (HasQueue env, HasCount ClueCount env InvestigatorId) => RunMessage env SearchForTheTruth where
  runMessage msg e@(SearchForTheTruth attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      clueCount' <- unClueCount <$> getCount iid
      e <$ pushAll
        [DrawCards iid (min 5 clueCount') False, Discard (toTarget attrs)]
    _ -> SearchForTheTruth <$> runMessage msg attrs
