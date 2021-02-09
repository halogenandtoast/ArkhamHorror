module Arkham.Types.Event.Cards.SearchForTheTruth where


import Arkham.Types.Event.Attrs

newtype SearchForTheTruth = SearchForTheTruth EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruth :: InvestigatorId -> EventId -> SearchForTheTruth
searchForTheTruth iid uuid = SearchForTheTruth $ baseAttrs iid uuid "02008"

instance HasModifiersFor env SearchForTheTruth where
  getModifiersFor = noModifiersFor

instance HasActions env SearchForTheTruth where
  getActions i window (SearchForTheTruth attrs) = getActions i window attrs

instance (HasQueue env, HasCount ClueCount env InvestigatorId) => RunMessage env SearchForTheTruth where
  runMessage msg e@(SearchForTheTruth attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      clueCount' <- unClueCount <$> getCount iid
      e <$ unshiftMessages
        [DrawCards iid (min 5 clueCount') False, Discard (toTarget attrs)]
    _ -> SearchForTheTruth <$> runMessage msg attrs
