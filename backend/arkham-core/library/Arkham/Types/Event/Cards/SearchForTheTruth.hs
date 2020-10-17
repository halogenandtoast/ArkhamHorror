{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.SearchForTheTruth where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Query
import Lens.Micro

import ClassyPrelude

newtype SearchForTheTruth = SearchForTheTruth Attrs
  deriving newtype (Show, ToJSON, FromJSON)

searchForTheTruth :: InvestigatorId -> EventId -> SearchForTheTruth
searchForTheTruth iid uuid = SearchForTheTruth $ baseAttrs iid uuid "02008"

instance HasActions env SearchForTheTruth where
  getActions i window (SearchForTheTruth attrs) = getActions i window attrs

instance (HasQueue env, HasCount ClueCount InvestigatorId env) => RunMessage env SearchForTheTruth where
  runMessage msg (SearchForTheTruth attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      clueCount' <- unClueCount <$> asks (getCount iid)
      unshiftMessage (DrawCards iid (min 5 clueCount') False)
      SearchForTheTruth <$> runMessage msg (attrs & resolved .~ True)
    _ -> SearchForTheTruth <$> runMessage msg attrs
